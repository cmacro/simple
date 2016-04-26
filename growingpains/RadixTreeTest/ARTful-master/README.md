ARTful
======

[ref]:https://github.com/malbrain/ARTful

ARTful radix tree preliminary version with initial support for inserts and finds only.

ARTfulkv.c:	initial version with a latch for every slot in the radix trie.

ARTfulkv2.c: advanced multi-threaded version that only latches one slot per insert
ARTfulkv3.c: clean-up, move slot type to slot entry. Revamp cmd interface.
ARTfulkv4.c: more clean-up.
ARTfulkv5.c: Rework node locking.
ARTfulkv6.c: experimental version.

To compile the source code:
cc -c ARTfulkv5.c -o ARTfulkv5 -O3 -lpthread

To time 16M inserts and finds of 4 byte random keys by a single thread:
./ARTfulkv5 xyz 4x 16000000

To time 16M inserts and finds of 4 byte random keys with 4 threads:
./ARTfulkv5 xyz 4x 4000000 4000000 4000000 4000000

To time sorting and subsequent finding of 10 byte key pennysort key segments:
./ARTfulkv5 xyz wf pennysortkeys*

To time sorting and subsequent listing of 10 byte key pennysort file segments:
./ARTfulkv5 xyz ps pennysort*

To time sorting and subsequent listing of two arbitrary files:
./ARTfulkv5 xyz ws file1 file2

An experimental version ARTfulkv6.c has better performance and a smaller footprint than ARTfulkv5.
