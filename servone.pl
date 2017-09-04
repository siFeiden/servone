:- use_module(library(readutil)).
:- use_module(library(socket)).

:- initialization(main).


http_status(200, 'OK').
http_status(302, 'Found').
http_status(500, 'Internal Server Error').


main :-
  current_prolog_flag(argv, [Host, PortA, ServedFile]),
  atom_number(PortA, Port),
  start_server(Host, Port, ServedFile).


start_server(Host, Port, ServedFile) :-
  tcp_socket(Socket),
  tcp_bind(Socket, Host:Port),
  tcp_listen(Socket, 5),
  tcp_open_socket(Socket, AcceptFd, _),
  dispatch(AcceptFd, ServedFile),
  close(AcceptFd).


dispatch(Socket, ServedFile) :-
  tcp_accept(Socket, Connection, _),
  thread_create(
    handle_request(Connection, ServedFile),
    _, [detached(true)]
  ),
  dispatch(Socket, ServedFile).


handle_request(Connection, ServedFile) :-
  tcp_open_socket(Connection, StreamPair),
  stream_pair(StreamPair, In, Out),
  catch(
    (
      parse_request(In, Path),
      make_request_answer(Path, ServedFile, Answer),
      write(Out, Answer)
    ),
    E,
    (
      writeln(E),
      send_500(Out)
    )
  ),
  close(StreamPair).


parse_request(In, Path) :-
  read_line_to_codes(In, RequestCodes),
  string_codes(Request, RequestCodes),
  writeln(Request),
  split_string(Request, " ", " ", ["GET", Path, "HTTP/1.1"]).
parse_request(_, _) :- throw(parse_failed).


make_request_answer(Path, ServedFile, Answer) :-
  format(string(Path), '/~s', ServedFile),
  !,
  setup_call_cleanup(
    open(ServedFile, read, FileIn),
    (
      read_string(FileIn, Length, FileContent),
      make_http200_headers(Length, Headers),
      format(string(Answer), '~s\r\n~s\r\n\r\n', [Headers, FileContent])
    ),
    close(FileIn)
  ).

make_request_answer(_, ServedFile, Answer) :-
  make_http302_headers(ServedFile, 0, Headers),
  format(string(Answer), '~s\r\n~i\r\n\r\n', [Headers, 'hello']).


make_http200_headers(Length, Headers) :-
  format(string(H), "Connection: Close\r\nContent-Type: text/plain\r\nContent-Length: ~d\r\n", Length),
  make_full_http_header(200, H, Headers).

make_http302_headers(Location, Length, Headers) :-
  format(string(H), "Connection: Close\r\nContent-Type: text/plain\r\nContent-Length: ~d\r\nLocation: /~s\r\n", [Length, Location]),
  make_full_http_header(302, H, Headers).


make_full_http_header(Code, Headers, Answer) :-
  http_status(Code, Message),
  format(string(Answer), 'HTTP/1.1 ~d ~s\r\n~s', [Code, Message, Headers]).

send_500(Out) :-
  make_full_http_header(500, "", Headers),
  write(Out, Headers).

