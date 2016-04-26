//	ARTful key-value store

//	Author: Karl Malbrain, malbrain@cal.berkeley.edu
//	Date:   23 DEC 14

/*
This work, including the source code, documentation
and related data, is placed into the public domain.

The orginal author is Karl Malbrain.

THIS SOFTWARE IS PROVIDED AS-IS WITHOUT WARRANTY
OF ANY KIND, NOT EVEN THE IMPLIED WARRANTY OF
MERCHANTABILITY. THE AUTHOR OF THIS SOFTWARE,
ASSUMES _NO_ RESPONSIBILITY FOR ANY CONSEQUENCE
RESULTING FROM THE USE, MODIFICATION, OR
REDISTRIBUTION OF THIS SOFTWARE.
*/

#include <stdlib.h>
#include <memory.h>
#include <string.h>
#include <stdio.h>

#include <xmmintrin.h>
#include <linux/futex.h>
#include <sys/syscall.h>

typedef unsigned char uchar;
typedef unsigned int uint;

enum NodeType {
	UnusedNode = 0, // node is not yet in use
	SpanNode,		// node contains key bytes (up to 16) and leaf element
	LeafPtr,		// node points to a leaf element
	Array4,			// node contains 4 radix slots & leaf element
	Array16,		// node contains 16 radix slots & leaf element
	Array64,		// node contains 64 radix slots & leaf element
	Array256		// node contains 256 radix slots & leaf element
};

typedef struct {
	void *node;			// node sub-contents
	uchar mutex[1];		// manipulation latch
	uchar nslot;		// number of slots in radix array in use
	uchar type;			// type of radix node
	uchar min;			// unused span bytes
} ARTslot;

typedef struct {
	void *value;		// leaf value that ended before this node's keys
	ARTslot radix[4];
	uchar keys[4];
} ARTnode4;

typedef struct {
	void *value;		// leaf value that ended before this node's keys
	ARTslot radix[16];
	uchar keys[16];
} ARTnode16;

typedef struct {
	void *value;		// leaf value that ended before this node's keys
	ARTslot radix[64];
	uchar keys[256];
} ARTnode64;

typedef struct {
	void *value;		// leaf value that ended before this node's keys
	ARTslot radix[256];
} ARTnode256;

typedef struct {
	void *value;		// leaf value that ended before this node's keys
	ARTslot next[1];	// next node after span
	uchar bytes[8];
} ARTspan;

typedef struct {
	ARTslot root[1];
} ARTtrie;

typedef struct {
	uchar len;			// this can be changed to a ushort or uint
	uchar value[0];
} ARTval;

typedef struct {
	ARTslot *slot;		// current slot
	uint off;			// offset within key
	int idx;			// current index within slot
} ARTstack;

typedef struct {
	uint maxdepth;		// maximum depth of ARTful trie
	uint depth;			// current depth of cursor
	ARTval *value;		// current leaf node
	ARTstack stack[0];	// cursor stack
} ARTcursor;

int sys_futex(void *addr1, int op, int val1, struct timespec *timeout, void *addr2, int val3)
{
	return syscall(SYS_futex, addr1, op, val1, timeout, addr2, val3);
}

#define relax() asm volatile("pause\n": : : "memory")

void mutexlock(uchar *latch)
{
  while( __sync_lock_test_and_set (latch, 1) )
	while( latch[0] )
		relax();
}

void mutexrelease(uchar *latch)
{
	__sync_synchronize();
	*latch = 0;
}

void slotlock (ARTslot *slot)
{
	if( (volatile)slot->type == Array256 )
		return;

	mutexlock (slot->mutex);

	if( (volatile)slot->type < Array256 )
		return;

	mutexrelease (slot->mutex);
}

unsigned long int ArenaOffset = 1024UL * 1024UL*1024UL *12;
uchar ArenaMutex[1];
uchar *Arena;

uint Census[8];

void art_free (ARTtrie *trie, uchar type, void *what)
{
	mutexlock (ArenaMutex);
	Census[type]--;
	mutexrelease (ArenaMutex);
}

void *art_node (ARTtrie *trie, uchar type)
{
uint size, xtra;
void *node;

	switch( type ) {
	case SpanNode:
		size = sizeof(ARTspan);
		break;
	case Array4:
		size = sizeof(ARTnode4);
		break;
	case Array16:
		size = sizeof(ARTnode16);
		break;
	case Array64:
		size = sizeof(ARTnode64);
		break;
	case Array256:
		size = sizeof(ARTnode256);
		break;
	}

	if( xtra = size & 0x7 )
		size += 8 - xtra;

	if( ArenaOffset < size )
		abort();

	mutexlock (ArenaMutex);
	Census[type]++;

	ArenaOffset -= size;
	node = Arena + ArenaOffset;
	mutexrelease (ArenaMutex);

	memset (node, 0, size);
	return node;
}

ARTcursor *ARTnewcursor (uint depth)
{
ARTcursor *cursor = calloc (1, sizeof(ARTcursor) + depth * sizeof(ARTstack));
	cursor->maxdepth = depth;
	return cursor;
}

ARTtrie *ARTnew ()
{
ARTtrie *trie = calloc (1, sizeof(ARTtrie));
ARTnode256 *radix256, *root256;
uint idx;

	Arena = malloc(ArenaOffset);
	return trie;
}

void ARTclose (ARTtrie *trie)
{
}

//	position cursor at largest key

void ARTlastkey (ARTtrie *trie, ARTcursor *cursor, uchar *key, uint keylen)
{
}

//	position cursor before requested key

void ARTstartkey (ARTtrie *trie, ARTcursor *cursor, uchar *key, uint keylen)
{
}

//	retrieve next key from cursor

uint ARTnextkey (ARTtrie *trie, ARTcursor *cursor, uchar *key, uint keymax)
{
}

//	retrieve previous key from cursor

uint ARTprevkey (ARTtrie *trie, ARTcursor *cursor, uchar *key, uint keymax)
{
}

//  find key in ARTful trie, returning current value or NULL

void *ARTfindkey (ARTtrie *trie, uchar *key, uint keylen)
{
ARTslot *prev, *slot;
uint len, idx, min;
ARTnode4 *radix4;
ARTnode16 *radix16;
ARTnode64 *radix64;
ARTnode256 *radix256;
void *oldvalue;
ARTspan *span;
uint off = 0;

	slot = trie->root;
	slotlock (slot);

	while( prev = slot, off < keylen && slot->type )
	  switch( slot->type ) {
	  case LeafPtr:
		mutexrelease (prev->mutex);
		return NULL;

	  case SpanNode:
		span = (ARTspan*)slot->node;
		len = slot->nslot;
		min = slot->min;

		if( len != keylen - off + min )
			return NULL;

		for( idx = 0; idx < len - min; idx++ )
		  if( key[off + idx] != span->bytes[min + idx] ) {
	  		mutexrelease (slot->mutex);
			return NULL;
		  }

		off += idx;
		slot = span->next;
		slotlock (slot);
		mutexrelease (prev->mutex);
		continue;

	  case Array4:
		radix4 = (ARTnode4*)slot->node;
		len = slot->nslot;

		for( idx = 0; idx < len; idx++ )
		  if( key[off + idx] == radix4->keys[idx] )
			break;

		if( idx == len ) {
		  mutexrelease (prev->mutex);
		  return NULL;
		}

		slot = radix4->radix + idx;
		slotlock (slot);

		mutexrelease (prev->mutex);
		off++;
		continue;

	  case Array16:
		radix16 = (ARTnode16*)slot->node;
		len = slot->nslot;

		for( idx = 0; idx < len; idx++ )
		  if( key[off] == radix16->keys[idx] )
			break;

		if( idx == len ) {
		  mutexrelease (prev->mutex);
		  return NULL;
		}

		slot = radix16->radix + idx;
		slotlock (slot);

		mutexrelease (prev->mutex);
		off++;
		continue;

	  case Array64:
		radix64 = (ARTnode64*)slot->node;

		if( radix64->keys[key[off]] == 0xff ) {
		  mutexrelease (prev->mutex);
		  return NULL;
		}

		slot = radix64->radix + radix64->keys[key[off++]];
		slotlock (slot);

		mutexrelease (prev->mutex);
		continue;

	  case Array256:
		radix256 = (ARTnode256*)slot->node;
		slot = radix256->radix + key[off++];

		if( !slot->type ) {
		  mutexrelease (prev->mutex);
		  return NULL;
		}

		slotlock (slot);

		mutexrelease (prev->mutex);
		continue;
	  }

	if( slot->type == LeafPtr ) {
	  if( oldvalue = slot->node ) {
		mutexrelease (prev->mutex);
		return oldvalue;
	  }
	}

	mutexrelease (prev->mutex);
	return NULL;
}

//	insert key/value into ARTful trie, returning old value

void *ARTinsert (ARTtrie *trie, uchar *key, uint keylen, void *value)
{
uint len, idx, idx2, min, max;
ARTslot *prev, *slot, *slot2;
ARTspan *span, *span2;
ARTnode4 *radix4;
ARTnode16 *radix16;
ARTnode64 *radix64;
ARTnode256 *radix256;
void *oldvalue;
uint off = 0;

	slot = trie->root;
	slotlock (slot);

	while( prev = slot, off < keylen && slot->type )
	  switch( slot->type ) {
	  case SpanNode:
		span = (ARTspan*)slot->node;
		max = len = slot->nslot;
		min = slot->min;

		if( len > keylen - off + min )
			len = keylen - off + min;

		for( idx = 0; idx < len - min; idx++ )
		  if( key[off + idx] != span->bytes[min + idx] )
			break;

		off += idx;

		// did we use the entire span node?

		if( idx + min == max ) {
		  slot = span->next;
		  slotlock (slot);
		  mutexrelease (prev->mutex);
		  continue;
		}

		// break span node into two parts
		// with radix node in between

		radix4 = art_node(trie, Array4);
		radix4->keys[0] = span->bytes[idx + min];

		if( span->next->type == LeafPtr )
		  radix4->value = slot->node;

		// truncate original span node to prefix bytes

		if( idx ) {
		  slot->nslot = min + idx;
		  slot = span->next;
		}

		// else cut it from the tree by transforming
		// the original slot to radix type

		else {
		  slot->nslot = 1 + (off < keylen);
		  slot->type = Array4;
		  slot->min = 0;
		}

		slot->node = radix4;

		// are there any original span bytes remaining?
		// place them under first radix branch

		if( idx + min + 1 < max )
		 if( idx ) {
		  span2 = art_node(trie, SpanNode);
		  slot = radix4->radix + 0;	// first radix element
		  *span2 = *span;
		  slot->nslot = max;
		  slot->node = span2;
		  slot->type = SpanNode;
		  slot->min = min + idx + 1;
		 } else {
		  slot = radix4->radix + 0;	// first radix element
		  slot->node = span;
		  slot->nslot = max;
		  slot->type = SpanNode;
		  slot->min = min + idx + 1;
		} else if( !idx )
		  art_free (trie, SpanNode, span);	// free span node

		// does our key terminate at the radix node?

		if( off == keylen ) {
		  radix4->value = value;
		  mutexrelease (prev->mutex);
		  return NULL;
		}

		// otherwise there are two radix elements
		// loop to process the second

		slot = radix4->radix + 1;	// second radix element
		radix4->keys[1] = key[off++];

		slotlock (slot);
		mutexrelease (prev->mutex);
		continue;

	  case LeafPtr:
		radix4 = art_node(trie, Array4);
		radix4->value = slot->node;
		radix4->keys[0] = key[off++];

		// transform LeafPtr slot into radix4 slot

		slot->nslot = 1;
		slot->node = radix4;
		slot->type = Array4;
		slot = radix4->radix + 0;

		slotlock (slot);
		mutexrelease (prev->mutex);
		continue;

	  case Array4:
		radix4 = (ARTnode4*)slot->node;

		for( idx = 0; idx < slot->nslot; idx++ )
		  if( key[off] == radix4->keys[idx] )
			break;

		if( idx < slot->nslot ) {
		  slot = radix4->radix + idx;
		  slotlock (slot);
		  mutexrelease (prev->mutex);
		  off++;
		  continue;
		}

		// add to radix node if room

		if( slot->nslot < 4 ) {
		  radix4->keys[slot->nslot] = key[off++];
		  slot = radix4->radix + slot->nslot++;
		  slotlock (slot);
		  mutexrelease (prev->mutex);
		  continue;
		}

		// the radix node is full, promote to
		// the next larger size.

		radix16 = art_node(trie, Array16);

		for( idx = 0; idx < slot->nslot; idx++ ) {
		  slot2 = radix4->radix + idx;
		  slotlock (slot2);
		  radix16->radix[idx] = *slot2;
		  *radix16->radix[idx].mutex = 0;
		  radix16->keys[idx] = radix4->keys[idx];
		  mutexrelease (slot2->mutex);
		}

		radix16->keys[slot->nslot] = key[off++];
		radix16->value = radix4->value;

		//  free the old Array4 node and install
		//  the new Array16 node into the trie.

		art_free (trie, Array4, slot->node);
		slot->type = Array16;
		slot->node = radix16;

		slot = radix16->radix + slot->nslot++;
		slotlock (slot);
		mutexrelease (prev->mutex);
		continue;

	  case Array16:
		radix16 = (ARTnode16*)slot->node;

		for( idx = 0; idx < slot->nslot; idx++ )
		  if( key[off] == radix16->keys[idx] )
			break;

		// key byte is in radix node

		if( idx < slot->nslot ) {
		  slot = radix16->radix + idx;
		  slotlock (slot);
		  mutexrelease (prev->mutex);
		  off++;
		  continue;
		}

		// add to radix node

		if( slot->nslot < 16 ) {
		  radix16->keys[slot->nslot] = key[off++];
		  slot = radix16->radix + slot->nslot++;
		  slotlock (slot);
		  mutexrelease (prev->mutex);
		  continue;
		}

		// the radix node is full, promote to
		// the next larger size.

		radix64 = art_node(trie, Array64);
		memset (radix64->keys, 0xff, sizeof(radix64->keys));

		for( idx = 0; idx < slot->nslot; idx++ ) {
		  slot2 = radix16->radix + idx;
		  slotlock (slot2);
		  idx2 = radix16->keys[idx];
		  radix64->radix[idx] = *slot2;
		  *radix64->radix[idx].mutex = 0;
		  radix64->keys[idx2] = idx;
		  mutexrelease (slot2->mutex);
		}

		radix64->keys[key[off++]] = slot->nslot;
		radix64->value = radix16->value;

		//  free the old Array16 node and install
		//  the new Array64 node into the trie.

		art_free (trie, Array16, slot->node);
		slot->type = Array64;
		slot->node = radix64;

		slot = radix64->radix + slot->nslot++;
		slotlock (slot);
		mutexrelease (prev->mutex);
		continue;

	  case Array64:
		radix64 = (ARTnode64*)slot->node;

		// is key already in radix node?

		if( radix64->keys[key[off]] < 0xff ) {
		  slot = radix64->radix + radix64->keys[key[off++]];
		  slotlock (slot);
		  mutexrelease (prev->mutex);
		  continue;
		}

		// add to radix node

		if( slot->nslot < 64 ) {
		  radix64->keys[key[off++]] = slot->nslot;
		  slot = radix64->radix + slot->nslot++;
		  slotlock (slot);
		  mutexrelease (prev->mutex);
		  continue;
		}

		// the radix node is full, promote to
		// the next larger size.

		radix256 = art_node(trie, Array256);

		for( idx = 0; idx < 256; idx++ )
		 if( radix64->keys[idx] < 0xff ) {
		  idx2 = radix64->keys[idx];
		  slot2 = radix64->radix + idx2;
		  slotlock (slot2);
		  radix256->radix[idx] = *slot2;
		  *radix256->radix[idx].mutex = 0;
		  mutexrelease (slot2->mutex);
		  }

		radix256->value = radix64->value;

		//  free the old Array64 node and install
		//  the new Array256 node into the trie.

		art_free (trie, Array64, slot->node);
		slot->type = Array256;
		slot->node = radix256;

		//  leave the Array256 slot unlocked

		slot = radix256->radix + key[off++];
		slotlock (slot);
		mutexrelease (prev->mutex);
		continue;

	  case Array256:
		radix256 = (ARTnode256*)slot->node;
		slot = radix256->radix + key[off++];
		slotlock (slot);  // don't unlock anything

		continue;
	}

	//	add span nodes to consume key

	while( len = keylen - off ) {
		span = art_node (trie, SpanNode);

		if( len > sizeof(span->bytes) )
			len = sizeof(span->bytes);

		slot->type = SpanNode;
		slot->node = span;
		slot->nslot = len;
		slot->min = 0;
		memcpy (span->bytes, key + off, len);
		slot = span->next;
		off += len;
	}

	if( slot->type == LeafPtr ) {
		oldvalue = slot->node;
		slot->node = value;
		mutexrelease(prev->mutex);
		return oldvalue;
	}

	if( slot->type ) {
		radix4 = (ARTnode4 *)slot->node;	// generic radix pointer
		oldvalue = radix4->value;
		radix4->value = value;
		mutexrelease(prev->mutex);
		return oldvalue;
	}

	// append a leaf ptr

	slot->type = LeafPtr;
	slot->node = value;
	mutexrelease(prev->mutex);
	return NULL;
}

#ifdef STANDALONE
#include <time.h>
#include <sys/resource.h>

double getCpuTime(int type)
{
struct rusage used[1];
struct timeval tv[1];

	switch( type ) {
	case 0:
		gettimeofday(tv, NULL);
		return (double)tv->tv_sec + (double)tv->tv_usec / 1000000;

	case 1:
		getrusage(RUSAGE_SELF, used);
		return (double)used->ru_utime.tv_sec + (double)used->ru_utime.tv_usec / 1000000;

	case 2:
		getrusage(RUSAGE_SELF, used);
		return (double)used->ru_stime.tv_sec + (double)used->ru_stime.tv_usec / 1000000;
	}

	return 0;
}

typedef struct {
	char idx;
	char *type;
	char *infile;
	ARTtrie *trie;
} ThreadArg;

#define ARTmaxkey 256
#define ARTdepth 256

//  standalone program to index file of keys
//  then list them onto std-out

void *index_file (void *arg)
{
int line = 0, found = 0, cnt = 0, cachecnt, idx;
int ch, len = 0, slot, type = 0;
unsigned char key[ARTmaxkey];
struct random_data buf[1];
ThreadArg *args = arg;
uint counts[8][2];
ARTcursor *cursor;
uchar state[64];
uint next[1];
int vallen;
ARTval *val;
uint size;
FILE *in;

	if( args->idx < strlen (args->type) )
		ch = args->type[args->idx];
	else
		ch = args->type[strlen(args->type) - 1];

	cursor = ARTnewcursor(ARTdepth);

	switch(ch | 0x20)
	{
	case '4':	// 4 byte random keys
		memset (buf, 0, sizeof(buf));
		initstate_r(args->idx * 100 + 100, state, 64, buf);
		for( line = 0; line < 16000000; line++ ) {
		random_r(buf, next);

			key[0] = next[0];
			next[0] >>= 8;
			key[1] = next[0];
			next[0] >>= 8;
			key[2] = next[0];
			next[0] >>= 8;
			key[3] = next[0];
			ARTinsert (args->trie, key, 4, NULL);
		}
		break;

	case 'd':
//		type = Delete;

	case 'p':
//		if( !type )
//			type = Unique;

//		 if( type == Delete )
//		  fprintf(stderr, "started pennysort delete for %s\n", args->infile);
//		 else
		  fprintf(stderr, "started pennysort insert for %s\n", args->infile);

		if( in = fopen (args->infile, "rb") )
		  while( ch = getc(in), ch != EOF )
			if( ch == '\n' )
			{
			  line++;

			  val = malloc (len - 10 + sizeof(ARTval));
			  memcpy (val->value, key + 10, len - 10);
			  val->len = len - 10;

			  if( ARTinsert (args->trie, key, 10, val) )
				  fprintf(stderr, "Duplicate key source: %d\n", line), exit(0);
			  len = 0;
			  continue;
			}

		    else if( len < ARTmaxkey )
			  key[len++] = ch;
		fprintf(stderr, "finished %s for %d keys\n", args->infile, line);
		break;

	case 'w':
		fprintf(stderr, "started indexing for %s\n", args->infile);
		if( in = fopen (args->infile, "r") )
		  while( ch = getc(in), ch != EOF )
			if( ch == '\n' )
			{
			  line++;

			  ARTinsert (args->trie, key, len, NULL);
			  len = 0;
			}
			else if( len < ARTmaxkey )
				key[len++] = ch;

		fprintf(stderr, "finished %s for %d keys\n", args->infile, line);
		break;

	case 'f':
		fprintf(stderr, "started finding keys for %s\n", args->infile);
		if( in = fopen (args->infile, "rb") )
		  while( ch = getc(in), ch != EOF )
			if( ch == '\n' )
			{
			  line++;
			  if( ARTfindkey (args->trie, key, len) )
				found++;
			  len = 0;
			}
			else if( len < ARTmaxkey )
				key[len++] = ch;
		fprintf(stderr, "finished %s for %d keys, found %d\n", args->infile, line, found);
		break;

	case 's':
		fprintf(stderr, "started forward scan\n");
		ARTstartkey (args->trie, cursor, NULL, 0);

		while( len = ARTnextkey (args->trie, cursor, key, ARTmaxkey) ) {
		  fwrite (key, len, 1, stdout);
		  val = cursor->value;
		  if( val->len )
			fwrite (val->value, val->len, 1, stdout);
		  fputc ('\n', stdout);
		  cnt++;
	    }

		fprintf(stderr, " Total keys read %d\n", cnt);
		break;

	case 'r':
		fprintf(stderr, "started reverse scan\n");
		ARTlastkey (args->trie, cursor, NULL, 0);

		while( len = ARTprevkey (args->trie, cursor, key, ARTmaxkey) ) {
		  fwrite (key, len, 1, stdout);
		  val = cursor->value;
		  if( val->len )
			fwrite (val->value, val->len, 1, stdout);
		  fputc ('\n', stdout);
		  cnt++;
	    }

		fprintf(stderr, " Total keys read %d\n", cnt);
		break;
	}

	free (cursor);
	return NULL;
}

typedef struct timeval timer;

int main (int argc, char **argv)
{
double start, stop;
pthread_t *threads;
int idx, cnt, err;
ThreadArg *args;
float elapsed;
void *trie;

	if( argc < 3 ) {
		fprintf (stderr, "Usage: %s idx_file cmds src_file1 src_file2 ... ]\n", argv[0]);
		fprintf (stderr, "  where idx_file is the name of the ARTful tree file\n");
		fprintf (stderr, "  cmds is a string of (r)ev scan/(w)rite/(s)can/(d)elete/(f)ind/(p)ennysort/(c)ount/(m)ainflush/(a)udit, with a one character command for each input src_file. A command can also be given with no input file\n");
		fprintf (stderr, "  src_file1 thru src_filen are files of keys or pennysort records separated by newline\n");
		exit(0);
	}

	start = getCpuTime(0);

	if( argc > 3 )
		cnt = argc - 3;
	else
		cnt = 0;

	threads = malloc (cnt * sizeof(pthread_t));
	args = malloc ((cnt + 1) * sizeof(ThreadArg));

//	triefd = open ((char*)argv[1], O_RDWR | O_CREAT, 0666);

//	if( triefd == -1 ) {
//		fprintf (stderr, "Unable to create/open ARTful file %s\n", argv[1]);
//		exit (1);
//	}

//	mgr = bt_mgr (cachefd, bits, leafxtra, poolsize);

//	if( !mgr ) {
//		fprintf(stderr, "Index Open Error %s\n", argv[1]);
//		exit (1);
//	} else {
//		mgr->type = 0;
//	}

	trie = ARTnew();

	//	fire off threads

	if( cnt > 1 )
	  for( idx = 0; idx < cnt; idx++ ) {
		args[idx].infile = argv[idx + 3];
		args[idx].type = argv[2];
		args[idx].trie = trie;
		args[idx].idx = idx;

		if( err = pthread_create (threads + idx, NULL, index_file, args + idx) )
			fprintf(stderr, "Error creating thread %d\n", err);
	  }
	else {
		args[0].infile = argv[3];
		args[0].type = argv[2];
		args[0].trie = trie;
		args[0].idx = 0;
		index_file (args);
	}

	// 	wait for termination

	if( cnt > 1 )
	  for( idx = 0; idx < cnt; idx++ )
		pthread_join (threads[idx], NULL);

	ARTclose (trie);

	elapsed = getCpuTime(0) - start;
	fprintf(stderr, " real %dm%.3fs\n", (int)(elapsed/60), elapsed - (int)(elapsed/60)*60);
	elapsed = getCpuTime(1);
	fprintf(stderr, " user %dm%.3fs\n", (int)(elapsed/60), elapsed - (int)(elapsed/60)*60);
	elapsed = getCpuTime(2);
	fprintf(stderr, " sys  %dm%.3fs\n", (int)(elapsed/60), elapsed - (int)(elapsed/60)*60);
}
#endif	//STANDALONE
