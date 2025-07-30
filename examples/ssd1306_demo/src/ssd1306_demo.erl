-module(ssd1306_demo).
-export([start/0]).

start() ->
  %% Start the SSD1306 display with default options
  Opts = [
    {scl, 9},
    {sda, 10},
    {clock_speed_hz, 400000},
    {peripheral, "i2c0"},
    {address, 16#3C},
    {height, 32},
%%    flip_vertical,
    flip_horizontal
  ],
  case ssd1306:start_link(Opts) of
    {ok, Pid} ->
%%      ssd1306:clear(Pid),
      io:format("SSD1306 display started with PID: ~p~n", [Pid]),
      ssd1306:set_inversion(Pid, false),
      ssd1306:set_contrast(Pid, 0),
      loop(Pid, 0, 1);
    Error ->
      Error
  end.

loop(Display, X = 128 - 10, 1) ->
  loop(Display, X - 1, -1);

loop(Display, 0, -1) ->
  loop(Display, 1, 1);

loop(Display, X, Inc) ->
  Bitmap = <<16#00, 16#ff, 16#81, 16#81, 16#81, 16#81, 16#81, 16#ff, 16#00>>,
  ssd1306:set_bitmap(Display, X, 16, 10, 8, Bitmap),
  ssd1306:set_bitmap(Display, 127 - 10 - X, 8, 10, 8, Bitmap),
  ssd1306:set_text(Display, left, top, <<"Hello World!">>),
  ssd1306:set_text(Display, left, bottom, <<"Bye!">>),
  FreeHeap = erlang:system_info(esp32_free_heap_size),
  HeapKB = FreeHeap / 1024,
  Formatted = io_lib:format("~.1f KB", [HeapKB]),
  ssd1306:set_text(Display, right, bottom, list_to_binary(Formatted)),
  timer:sleep(50),
  loop(Display, X + Inc, Inc).
