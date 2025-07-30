-module(ssd1306).
-behaviour(gen_server).

-type start_opt() ::
{scl, integer()} |
{sda, integer()} |
{clock_speed_hz, integer()} |
{peripheral, binary()} |
{address, integer()} |
{height, integer()} |
flip_vertical |
flip_horizontal.

-type horizontal_text_position() :: left | center | right | integer().
-type vertical_text_position() :: top | bottom | integer().

%% API
-export([start_link/1, clear/1, set_inversion/2, set_contrast/1, set_bitmap/6, set_text/4]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
  i2c,        % I2C port handle
  addr,       % I2C device address
  num_pages   % Number of pages (height / 8)
}).

%%% Control-byte macros
-define(CONTROL_CMD, 16#00).
-define(CONTROL_DATA, 16#40).

%%% Command macros
-define(CMD_DISPLAY_OFF, 16#AE).
-define(CMD_DISPLAY_ON, 16#AF).
-define(CMD_SET_CLOCK_DIV, 16#D5).
-define(CMD_SET_MULTIPLEX, 16#A8).
-define(CMD_SET_OFFSET, 16#D3).
-define(CMD_SET_START_LINE, 16#40).
-define(CMD_ENABLE_CHARGE_PUMP, 16#8D).
-define(CMD_MEMORY_MODE, 16#20).
-define(CMD_SEG_REMAP, 16#A0).
-define(CMD_COM_SCAN_DIR, 16#C8).
-define(CMD_SET_COM_PINS, 16#DA).
-define(CMD_CONTRAST, 16#81).
-define(CMD_PRECHARGE, 16#D9).
-define(CMD_VCOMH_DESEL, 16#DB).
-define(CMD_RESUME_RAM, 16#A4).
-define(CMD_NORMAL_DISPLAY, 16#A6).
-define(CMD_INVERSE_DISPLAY, 16#A7).
-define(CMD_SET_COLUMN_ADDRESS, 16#21).
-define(CMD_SET_PAGE_ADDRESS, 16#22).

%%%===================================================================
%%% Public API
%%%===================================================================

-spec start_link([start_opt()]) -> {ok, pid()} | {error, any()}.
start_link(Opts) ->
  gen_server:start_link(?MODULE, Opts, []).

-spec set_contrast(integer()) -> ok | {error, any()}.
set_contrast(Value) when is_integer(Value), Value >= 0, Value =< 255 ->
  gen_server:call(?MODULE, {set_contrast, Value});
set_contrast(_) ->
  {error, invalid_contrast}.

-spec set_inversion(pid(), boolean()) -> ok | {error, any()}.
set_inversion(Display, Invert) when is_boolean(Invert) ->
  ok = gen_server:cast(Display, {set_inversion, Invert}),
  ok.

-spec set_bitmap(pid(), integer(), integer(), integer(), integer(), binary()) -> ok | {error, any()}.
set_bitmap(Display, X, Y, Width, Height, Bitmap) ->
  gen_server:cast(Display, {set_bitmap, X, Y, Width, Height, Bitmap}).

-spec clear(pid()) -> ok | {error, any()}.
clear(Display) ->
  gen_server:cast(Display, clear).

-spec set_text(pid(), horizontal_text_position(), vertical_text_position(), binary()) -> ok |
{error, any()}.
set_text(Display, X, Y, Text) ->
  gen_server:cast(Display, {set_text, X, Y, Text}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

-spec init([start_opt()]) -> {ok, #state{}}.
init(Opts) ->
  SCL = proplists:get_value(scl, Opts),
  SDA = proplists:get_value(sda, Opts),
  Speed = proplists:get_value(clock_speed_hz, Opts, 400000),
  Peripheral = proplists:get_value(peripheral, Opts, <<"i2c0">>),
  Addr = proplists:get_value(address, Opts, 16#3C),
  Height = proplists:get_value(height, Opts, 32),
  I2C = i2c:open([{scl, SCL}, {sda, SDA}, {clock_speed_hz, Speed}, {peripheral, Peripheral}]),
  NumPages = Height div 8,
  FlipVertical = proplists:is_defined(flip_vertical, Opts),
  FlipHorizontal = proplists:is_defined(flip_horizontal, Opts),
  ok = init_display(I2C, Addr, NumPages, FlipVertical, FlipHorizontal),
  {ok, #state{i2c = I2C, addr = Addr, num_pages = NumPages}}.

handle_cast(clear, #state{i2c = I2C, addr = Addr, num_pages = NPages} =
  State) ->
  clear_display(I2C, Addr, NPages),
  {noreply, State};

handle_cast({set_inversion, ShouldInvert}, #state{i2c = I2C, addr = Addr} = State) ->
  set_display_inversion(I2C, Addr, ShouldInvert),
  {noreply, State};

handle_cast({set_bitmap, X, Y, W, H, Bitmap}, State = #state{i2c = I2C, addr = Addr}) ->
  do_set_bitmap(X, Y, W, H, Bitmap, I2C, Addr),
  {noreply, State};

handle_cast({set_text, X, Y, Text}, State = #state{i2c = I2C, addr = Addr, num_pages = NPages}) ->
  do_set_text(X, Y, Text, I2C, Addr, NPages),
  {noreply, State};

handle_cast(Msg, State) ->
  io:format("Received unknown cast message: ~p~n", [Msg]),
  {noreply, State}.

handle_call(_Msg, _From, State) ->
  {reply, ok, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, #state{i2c = I2C}) ->
  i2c:close(I2C),
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal helpers
%%%===================================================================

-spec init_display(port(), integer(), integer(), boolean(), boolean()) -> ok.
init_display(I2C, Addr, NumPages, FlipVertical, FlipHorizontal) ->
  ComPinsVal = case NumPages of
                 4 -> 16#02;  % 0x12 for 32 pixels height
                 8 -> 16#12;  % 0x22 for 64 pixels height
                 _ -> {error, invalid_height}
               end,
  Cmds = <<
    ?CONTROL_CMD, ?CMD_DISPLAY_OFF,           % 0xAE
    ?CONTROL_CMD, ?CMD_SET_CLOCK_DIV, 16#F0,  % 0xD5, 0x80
    ?CONTROL_CMD, ?CMD_SET_MULTIPLEX, (8 * NumPages - 1):8,  % 0xA8, multiplex ratio
    ?CONTROL_CMD, ?CMD_SET_OFFSET, 16#00,     % 0xD3, display offset
    ?CONTROL_CMD, ?CMD_SET_START_LINE,        % 0x40, start line = 0
    ?CONTROL_CMD, ?CMD_ENABLE_CHARGE_PUMP, 16#14,  % 0x8D, enable internal charge pump
    ?CONTROL_CMD, ?CMD_MEMORY_MODE, 16#01,    % 0x20, vertical addressing mode
    % Segment remap and COM scan direction - conditional for flipping
    ?CONTROL_CMD, (if FlipVertical -> 16#A1; true ->
      16#A0 end), % Segment remap (A1 for flip)
    ?CONTROL_CMD, (if FlipHorizontal -> 16#C0; true ->
      16#C8 end),   % COM scan dir (C0 for flip)
    ?CONTROL_CMD, ?CMD_SET_COM_PINS, ComPinsVal:8,  % 0xDA, COM pins hardware config
    ?CONTROL_CMD, ?CMD_CONTRAST, 16#CF,        % 0x81, contrast
    ?CONTROL_CMD, ?CMD_PRECHARGE, 16#F1,       % 0xD9, pre-charge
    ?CONTROL_CMD, ?CMD_VCOMH_DESEL, 16#40,     % 0xDB, VCOMH deselect
    ?CONTROL_CMD, ?CMD_RESUME_RAM,             % 0xA4, resume to RAM content display
    ?CONTROL_CMD, ?CMD_NORMAL_DISPLAY,         % 0xA6, normal (non-inverted) display
    ?CONTROL_CMD, ?CMD_DISPLAY_ON              % 0xAF
  >>,
  i2c:write_bytes(I2C, Addr, Cmds),
  clear_display(I2C, Addr, NumPages),
  ok.

-spec clear_display(port(), integer(), integer()) -> ok | {error, Reason :: term()}.
clear_display(I2C, Addr, NumPages) ->
  %% Build the 8-bit command header as a binary
  Cmds = <<
    ?CONTROL_CMD, ?CMD_SET_COLUMN_ADDRESS, 0:8, 127:8,
    ?CONTROL_CMD, ?CMD_SET_PAGE_ADDRESS, 0:8, (NumPages - 1):8
  >>,
  ZeroBytes = binary:copy(<<16#00>>, 128 * NumPages),
  i2c:write_bytes(I2C, Addr, Cmds),
  i2c:write_bytes(I2C, Addr, <<?CONTROL_DATA, ZeroBytes/binary>>),
  ok.

-spec set_display_inversion(port(), integer(), boolean()) -> ok | {error, Reason :: term()}.
set_display_inversion(I2C, Addr, ShouldInvert) ->
  Cmd = case ShouldInvert of
          true -> ?CMD_INVERSE_DISPLAY;
          false -> ?CMD_NORMAL_DISPLAY
        end,
  Cmds = <<?CONTROL_CMD:8, Cmd:8>>,
  i2c:write_bytes(I2C, Addr, Cmds).

-spec do_set_bitmap(integer(), integer(), integer(), integer(), binary(), port(), integer()) -> ok.
do_set_bitmap(X, Y, Width, Height, Bitmap, I2C, Addr) ->
  StartPage = Y div 8,
  EndPage = (Y + Height - 1) div 8,
  i2c:write_bytes(I2C, Addr, <<?CONTROL_CMD, ?CMD_SET_COLUMN_ADDRESS, X:8, (X + Width - 1):8>>),
  i2c:write_bytes(I2C, Addr, <<?CONTROL_CMD, ?CMD_SET_PAGE_ADDRESS, StartPage:8, EndPage:8>>),
  i2c:write_bytes(I2C, Addr, <<?CONTROL_DATA, Bitmap/binary>>),
  ok.

-spec do_set_text(horizontal_text_position(), vertical_text_position(), binary(), port(), integer(), integer()) -> ok.
do_set_text(X, Y, Text, I2C, Addr, NPages) ->
  %% Render glyphs into a binary bitmap
  Glyphs = [font:glyph(<<C>>) || C <- binary_to_list(Text)],
  Bitmap = iolist_to_binary(Glyphs),
  Width = byte_size(Bitmap),
  ActualX = case X of
    left -> 0;
    center -> (128 - Width) div 2;
    right -> 128 - Width;
    _ when is_integer(X) -> X
  end,
  ActualY = case Y of
    top -> 0;
    bottom -> (NPages - 1) * 8;
    _ when is_integer(Y) -> Y
  end,
  do_set_bitmap(ActualX, ActualY, Width, 8, Bitmap, I2C, Addr).