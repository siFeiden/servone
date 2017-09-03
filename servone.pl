:- use_module(library(socket)).

:- initialization(main).


main :-
  current_prolog_flag(argv, [Host, PortS, ServedFile]),
  atom_number(PortS, Port),
  start_server(Host, Port, ServedFile).

start_server(Host, Port, ServedFile) :-
  tcp_socket(Socket),
  tcp_bind(Socket, Host:Port),
  tcp_listen(Socket, 5),
  tcp_accept(Socket, Peer, _), % _ is PeerIp
  dispatch(Peer, ServedFile).

dispatch(Socket, ServedFile) :-
  setup_call_cleanup(
    tcp_open_socket(Socket, StreamPair),
    serve_request(StreamPair, ServedFile),
    close(StreamPair)
  ).

serve_request(StreamPair, ServedFile) :-
  stream_pair(StreamPair, _, O),
  open(ServedFile, read, FileIn, []),
  read_string(FileIn, Length, FileContent),
  write_http_headers(O, Length),
  write(O, FileContent),
  write(O, '\r\n\r\n').

write_http_headers(OutStream, Length) :-
  format(atom(S), 'HTTP/1.1 200 OK\r\nConnection: close\r\nContent-Type: text/plain\r\nContent-Length: ~d\r\n\r\n', Length),
  write(OutStream, S).
