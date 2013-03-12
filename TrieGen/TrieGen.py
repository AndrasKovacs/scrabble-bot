#!/usr/bin/env python

import os
import array as ar
import hashlib
from sys import argv
from copy import copy
from collections import defaultdict
from time import clock
import subprocess
import os

###### config ######

#edit this for custom dictionary
dict_file = "TWL06.txt"


temp_file = "trie.tmp"
output_file = r"..\trie.hsdat"

####################

class SeqTrie(object):
    def __init__(self, init = tuple(), is_end = False, val = "", end_of_list = False):
        self.children = []
        self.is_end = is_end
        self.val = val
        self.end_of_list = end_of_list
        for x in init: 
            self.add(x)

    def add(self, word):
        for c in word:
            if not self.children or self.children[-1].val != c: #only works on pre-sorted word lists! 
                self.children.append(SeqTrie())
            self = self.children[-1]
            self.val = c
        self.is_end = True

    def __iter__(self):
        for x in self.children:
            for y in x.__iter__():
                yield y
        yield self

wordlist = open(dict_file).read().split()
trie = SeqTrie(wordlist)

if not all(all(c.isupper() for c in w) for w in wordlist) or any(b < a for a,b in zip(wordlist, wordlist[1:])):
    print "Dictionary must be all uppercase and in alphabetical order"
    exit(1)

################### Generate hashes/merge nodes,  ###########################

node_dict = {}
for x in trie:
    hash_str = "".join((str(x.is_end), x.val, "".join(y.hash for y in x.children)))
    x.hash = hashlib.md5(hash_str).digest()
    if x.hash not in node_dict: 
        node_dict[x.hash] = x
        for i,y in enumerate(x.children):
            x.children[i] = node_dict[y.hash]
        x.children = tuple(sorted(x.children))

clist_dict = {x.children: x.children for x in node_dict.itervalues()}

for x in node_dict.itervalues():
    x.children = clist_dict[x.children]


########################## Merge child lists ###############################

inverse_dict = defaultdict(list)
compress_dict = {x:[x] for x in clist_dict.itervalues() if x}

for clist in clist_dict.itervalues():
    for node in clist:
        inverse_dict[node].append(clist)

for x in inverse_dict:
    inverse_dict[x].sort( key = lambda x: (len(x), sum(len(inverse_dict[y]) for y in x) ))

for clist in sorted(compress_dict.keys(), key = lambda x:(len(x), -1*sum(len(inverse_dict[y]) for y in x)), reverse=True):
    for other in min((inverse_dict[x] for x in clist), key = len):
        if compress_dict[other] and set(clist) < set(compress_dict[other][-1]):
            compress_dict[other].append(clist)
            compress_dict[clist] = False
            break

compress_dict = {x:l for x,l in compress_dict.iteritems() if l}


#################### Create compressed trie structure #######################


end_node = SeqTrie(init = (), is_end = False, val = "", end_of_list = True)
end_node.children = ()

array = [0,]*(sum(len(x[0]) for x in compress_dict.itervalues()) + 1)
clist_indices = {}
pos = 0
for stuff in compress_dict.itervalues():
    if len(stuff) > 1:
        sort = [0]*26
        for i, clist in enumerate(stuff):
            for y in clist:
                sort[ord(y.val) - ord('A')] = (i, y)
        stuff.append([n for i,n in sorted(x for x in sort if x)])
        for clist in stuff[:-1]:
            clist_indices[clist] = pos + len(stuff[0]) - len(clist)
    else:
        clist_indices[stuff[0]] = pos

    clist = stuff[-1]
    array[pos:pos+len(clist)] = map(copy, clist)
    pos += len(clist)
    array[pos-1].end_of_list = True

array[pos] = end_node
clist_indices[()] = pos

for x in array:
    x.children = clist_indices[x.children]

root = clist_indices[trie.children]
root_node = SeqTrie(init = (), is_end = False, val = "", end_of_list = True)
root_node.children = root
array.append(root_node)


######################### check trie ###################################


def extract_words(array, i=root, carry = ""):
    node = array[i]
    if not node.val:
        return
    while True:
        for x in extract_words(array, node.children, carry + node.val):
            yield x
        if node.is_end:
            yield carry + node.val
        if node.end_of_list: break
        i += 1
        node = array[i]

if set(extract_words(array)) != set(wordlist):
    print "INVALID OUTPUT: trie does not match original word list."
    exit(1)


################## export as bitpacked array #########################


output = ar.array('L', [0]*len(array))

for i,x in enumerate(array):
    output[i] |= (x.children << 10)
    output[i] |= ((ord(x.val) if x.val else 0) << 2)
    output[i] |= (x.end_of_list<<1)
    output[i] |= (x.is_end)
outfile = open(temp_file, "wb")
output.tofile(outfile)
outfile.close()


################## call Haskell converter ##########################

# Build it if necessary:
subprocess.call(["ghc", "--make", "-O2", "TrieGen.hs"])

# serialize to Haskell Binary format
subprocess.call(["TrieGen.exe", temp_file, output_file])
os.remove(temp_file)
