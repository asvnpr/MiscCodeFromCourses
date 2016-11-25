/*
    Alejandro Salvador Vega Nogales
    CCOM 4086-002
    Prof. C. Corrada
*/

#include "cachelab.h"
#include <stdlib.h>
#include <stdio.h>
#include <getopt.h>
#include <limits.h>
#include <stdbool.h>
#include <strings.h>

typedef unsigned long long address;

//struct to store the valid bit, tag, and data in each "slot" (line) of the cache
typedef struct
{
    bool valid;
    unsigned int lru_c; //count for lru
    address tag;
    address addr; //data
} cache_line;

typedef struct
{
    cache_line *lines;
} cache_set; //cache set with E lines. functionally acts as an array of cache lines

typedef struct
{
    cache_set *sets;
} cache; //cache of size S * E. functionally acts as an array of cache sets (or an array of an array of cache Lines)

typedef struct
{
    unsigned int line_index;
    unsigned int lru;
    unsigned int mru;
} lru_info;//struct for storing info from find_lru function

typedef struct
{
    int hits;
    int misses;
    int evictions;
} cache_result; //hits, misses, and evictions store the result of the operations in the given trace file

//s is s bits in the address after b bits. also denotes S(2^s) which is the number of Sets ("rows")
//E denotes the number of Lines (columns) in the cache (per set)
//b is the first b bits in the address. also denotes B(2^b) which is the block size for the data
int s, E, b, S, B; //arguments and values to store result of arguments
char c; //char to hold current argument in getopt loop
char *filename;
FILE *trace_file; //trace file to read operation, operation address, and size

cache initCache(); //function to initialize an empty cache
void delCache(cache cachoso); //deallocate memory from cache after we're done with it
lru_info find_lru(cache_set seti, lru_info info); //find least recently used line, it's index, and the most recently used line
cache_result cache_sim(cache cachi, address addr, cache_result resulti); //function to run cache operations and store the result in a struct to be returned



int main(int argc, char *argv[])
{
    bool verbty = 0; //flag for verbose output
    char op; //operation in each line of a trace file
    address addr; //address in each line of trace file
    int size; //size in each line of trace file

    while( (c=getopt(argc,argv,"s:E:vb:t:")) != -1)//get command line arguments
    {
        switch(c)//current argument in iteration
        {
            case('s'):
                s = atoi(optarg);
                S = 1 << s;
                break;
            case('E'):
                E = atoi(optarg);
                break;
            case('v'):
                verbty++;//if present in arguments turn on verbose flag
                break;
            case('b'):
                b = atoi(optarg);
                B = 1 << b;//set block size
                break;
            case('t'):
                filename = optarg;//get filename string for file that will be read
                break;

            default:
                printf("Invalid argument\n");
        }

    }//get values from command line argument

    //printf("got out of argv loop\n");

    if (s == 0 || E == 0 || b == 0 || filename == NULL)
    {
        printf("You need to enter a non-zero value for all arguments except v which is optional.\n");
        exit(1);
    }//error checking for missing arguments

    //load file to read cache operations, addresses, and sizes from
    trace_file = fopen(filename, "r");

    if (trace_file == NULL)
    {
        printf("Error reading trace file\n");
        exit(1);
    }//if file dne or error reading exit this program

    //initialize the struct for holding cache operation results
    cache_result res;
    res.hits = 0;
    res.misses = 0;
    res.evictions = 0;

    //same struct as above but stores the result of each iteration through the file (for verbose output)
    cache_result iter_res;
    iter_res.hits = 0;
    iter_res.misses = 0;
    iter_res.evictions = 0;

    cache cachetero = initCache();//initialize an empty cache with appropriate size(memory allocations) and default values

    //while the file can read a char(operation), a long long hex(address), and an int (size), keep reading each line
    while(fscanf(trace_file, " %c %llx,%d", &op, &addr, &size) == 3)
    {
        //char *op_s;
        cache_result tmp;
        switch(op)
        {
            case('I'):
                continue;
                break;
            case('L'):
                iter_res = cache_sim(cachetero, addr, iter_res);
                break;
            case('S'):
                iter_res = cache_sim(cachetero, addr, iter_res);
                break;
            case('M'):
                tmp = cache_sim(cachetero, addr, iter_res);
                iter_res = cache_sim(cachetero, addr, iter_res);
                iter_res.hits += tmp.hits;
                iter_res.evictions += tmp.evictions;
                iter_res.misses += tmp.misses;
                break;
            default:
                printf("error reading operation\n");
                break;
        }

        if(verbty)
        {
            /* "Each data load (L) or store (S) operation can cause at most one cache miss.
            The data modify operation (M) is treated as a load followed by a store to the same address.
            Thus, an M operation can result in two cache hits, or a miss and a hit plus a possible eviction."*/
            if(iter_res.evictions > 0 && iter_res.hits > 0) //case for M instruction
            {
                printf("%c %llx,%d eviction hit\n", op, addr, size);
            }
            else if(iter_res.misses > 0 && iter_res.hits > 0) //case for M instruction
            {
                printf("%c %llx,%d miss hit\n", op, addr, size);
            }
            else if(iter_res.misses > 0 && iter_res.evictions > 0) //case for M instruction
            {
                printf("%c %llx,%d miss eviction\n", op, addr, size);
            }
            else if(iter_res.hits > 0)
            {
                printf("%c %llx,%d hit\n", op, addr, size);
            }
            else if(iter_res.evictions > 0)
            {
                printf("%c %llx,%d eviction\n", op, addr, size);
            }
            else if(iter_res.misses > 0)
            {
                printf("%c %llx,%d miss\n", op, addr, size);
            }
        }

        //add iteration results to final results
        res.hits += iter_res.hits;
        res.misses += iter_res.misses;
        res.evictions += iter_res.evictions;

        //reset iteration results for each iteration
        iter_res.hits = 0;
        iter_res.misses = 0;
        iter_res.evictions = 0;
    }

    fclose(trace_file);
    delCache(cachetero);

    //skeleton csim code
    printSummary(res.hits, res.misses, res.evictions);
    return 0;
}


cache initCache()
{
    cache cachi;
    cache_set seti;
    cache_line liney;

    //initialize the cache_set of the cache by allocating memory according to the number of sets (S)
    cachi.sets = (cache_set *) malloc(sizeof(cache_set) * S);

    //cache has array of S sets(initialized above).
    //go through sets, allocate memory for E lines, and initialize each line in the set with default values
    for (int si = 0; si < S; si++)
    {
        seti.lines = (cache_line *) malloc(sizeof(cache_line) * E); //allocate memory for E number of cache_lines per set
        cachi.sets[si] = seti; //set index si(set index) of set to initialized set from above

        //go through each line in set index si, initialize default values and allocate memory for block of data (not super sure about this last part)
        for (int li = 0; li < E; li++)
        {
            //default values for each cache line or "slot"
            liney.valid = false;
            liney.lru_c = 0;
            liney.tag = 0;
            //liney.block = (char *) malloc(sizeof(char) * B);
            seti.lines[li] = liney;//set number li is initialized with default cache_line from above
        }
    }

    return cachi; //return our initialized cache
}

void delCache(cache cachoso)
{
    for (int si = 0; si < S; si++)
    {
        cache_set seti = cachoso.sets[si];
        free(seti.lines);
    }//deallocate the array of cache lines in each set (by iterating through the set array)
    free(cachoso.sets); //once the memory held by the lines in each set has been freed we can deallocate memory for the sets
}

lru_info find_lru(cache_set seti, lru_info info)
{
    info.lru = seti.lines[0].lru_c;//set the lru counter
    //info.set_index = 0;
    info.line_index = 0;

    for (int li = 1; li < E; li++)
    {
        //if you find an "older"(smaller) lru store the indexes of the set and line, and the lru
        if(seti.lines[li].lru_c < info.lru)
        {
            info.lru = seti.lines[li].lru_c;
            //info.set_index = si;
            info.line_index = li;
        }
        if(seti.lines[li].lru_c > info.mru)
        {
            info.mru = seti.lines[li].lru_c; //update mru until we find the most recently used cache line
        }
    }

    return info;//return lru info
}

cache_result cache_sim(cache cachi, address addr, cache_result resulti)
{
    bool has_empty = false; //flag to know if there's an empty line
    int empty_line_index = -1; //invalid index until an empty line is found
    int past_hits = resulti.hits; //keep track of past hits before operation

    int t_size = (64 - b - s );//get num of tag bits
    unsigned long long set_index = (addr << t_size) >> (t_size + b); //get s bits from address to get set index to place data

    address tagi = addr >> (s + b);//get tag to place in cache

    cache_set seti = cachi.sets[set_index];

    for (int li = 0; li < E; li++) //go through selected set lines and see if tag is present
    {
        cache_line liney = seti.lines[li];

        if(liney.valid && liney.tag == tagi)
        {
            liney.lru_c++;
            resulti.hits++;
            seti.lines[li] = liney;
        }//we found the value in the cache so updted lru, update hits, and save hit line

        else if(!liney.valid && !has_empty)
        {
            empty_line_index = li;
            has_empty++;
        }//there's an empty line so update full cache flag
    }

    if (past_hits == resulti.hits)//hits didn't increment so we had a miss
    {
        resulti.misses++;//update misses
    }
    else
    {
        return resulti;//if hits incremented then address was found and we can return the cache result
    }

    //cache had a miss so update cache by storing or by evicting

    //initialize a lru struct to use it to store lru info from function to find lru
    lru_info curr_info;
    curr_info.line_index = 0;
    curr_info.lru = INT_MAX;//make sure we find smaller lru
    curr_info.mru = INT_MIN;//make sure we find larger mru

    curr_info = find_lru(seti, curr_info); //get lru from current set

    if(!has_empty && empty_line_index == -1)
    {
        resulti.evictions++;
        //update cache by evicting. cache was full so we don't need to update valid flag
        seti.lines[curr_info.line_index].tag = tagi;
        seti.lines[curr_info.line_index].lru_c = curr_info.mru + 1;
        seti.lines[curr_info.line_index].addr = addr;
    }//set doesn't have an empty line and empty_line_index is invalid. evict by overriding the values of the lru line
    else
    {
        //int empty_line_index = check_empty;
        //write values to empty line in set
        seti.lines[empty_line_index].valid = true;
        seti.lines[empty_line_index].tag = tagi;
        seti.lines[empty_line_index].lru_c = curr_info.mru + 1;
        seti.lines[curr_info.line_index].addr = addr;
    }//we found an empty line. set the line with correct address and values

    return resulti;
}//simulating cache operations and returning resuls in appropriate struct
