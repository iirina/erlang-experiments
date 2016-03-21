Create 2 nodes:

$ sudo erl -sname node1@localhost

$ sudo erl -sname node2@localhost

In node2@localhost:

$ L1=[1,2,3].

$ L2=[4,5,6].

$ myrpc:call(node1@localhost,lists,append,[L1,L2]).
