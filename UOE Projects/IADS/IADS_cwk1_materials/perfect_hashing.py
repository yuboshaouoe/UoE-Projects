# Inf2-IADS Coursework 1, October 2019, revised October 2021
# Python source file: perfect_hashing.py
# Author: John Longley

# PART B: A STATE-OF-THE-ART PERFECT HASHING SCHEME
# (template file)

# Adapting a method of Belazzougui, Botelho and Dietzfelbinger (2008)


# Start with very crude 'mod' hashing.
# First, let's read a lowercase word as a base 27 integer:
from index_build import MetaIndex


def toInt(w):
    b = w.encode()
    t = 0
    for i in range(len(b)):
        t = t * 27 + b[i] - 96
    return t


# Simple mod hash with some scrambling.
# (We want hashes mod p,p' to be 'independent' when p != p'.)
# We shall take p prime for the outer hash, but not necessarily the inner ones.

def modHash(s, p):
    return (toInt(s) * 21436587 + 12345678912345) % p


# Classic 'bucket array' hash table:

def buildHashTable(L, h, r):
    table = [[] for i in range(r)]
    for w in L:
        table[h(w)].append(w)
    return table


def buildModHashTable(L, p):
    return buildHashTable(L, lambda w: modHash(w, p), p)
    # :worth trying out for small L and p


# Finding a suitable prime for the outer hash:

def isPrime(n):
    if n % 2 == 0 and n != 2:
        return False
    else:
        j = 3
        while j * j <= n:
            if n % j == 0:
                return False
            else:
                j += 2
        else:
            return True


def prevPrime(n):
    if n % 2 == 0:
        return prevPrime(n - 1)
    elif isPrime(n):
        return n
    else:
        return prevPrime(n - 2)


# For the mini-hashes, the following very simple enumeration works just fine
# (moduli needn't be prime, but we at least avoid multiples of 2 or 3)
# Results will later be further reduced modulo m (main table size).

def miniHash(m, j):
    d = j * 6 + 3000001
    return (lambda w: modHash(w, d) % m)


# TODO:
# Add your code here.
def hashCompress(L, m):
    indexed_L = list(enumerate(L))  # get the original list index
    indexed_L.sort(key=lambda bucket: len(bucket[1]), reverse=True)  # sort and reverse the list
    T, R = [False] * m, [0] * len(L)
    for B in indexed_L:  # B = (bucket,index)
        j = 0
        while True:
            # find all unoccupied minihash values
            elementHashes = [miniHash(m, j)(element) for element in B[1]
                             if not T[miniHash(m, j)(element)]]
            # check for duplicates and if all entries in bucket have a viable minihash
            if len(set(elementHashes)) != len(B[1]):
                j += 1
            else:  # set T slots to be occupied and save j to R
                for h in elementHashes:
                    T[h] = True
                R[B[0]] = j
                break
    return R


# Putting it all together:
# compact data structure for representing a perfect hash function


class Hasher:
    def __init__(self, keys, lam, load):
        # keys : list of keys to be hashed
        # lam  : load on outer table, i.e. average bucket size
        #        (higher lam means more compression but
        #        perfect hash function may be harder to construct)
        # load : desired load on resulting hash table, must be < 1
        # hashEnum : enumeration of hash functions used (e.g. miniHash)
        self.n = len(keys)
        self.r = prevPrime(int(self.n // lam))
        self.m = int(self.n // load)
        HT = buildModHashTable(keys, self.r)
        self.hashChoices = hashCompress(HT, self.m)
        # :results in a very small data structure with no trace of keys!

    def hash(self, key):
        i = modHash(key, self.r)
        h = miniHash(self.m, self.hashChoices[i])
        return h(key)


# We can double-check that our hash function really is perfect
# by building the corresponding ordinary hash table:

def checkPerfectHasher(keys, H):
    T = buildHashTable(keys, lambda key: H.hash(key), H.m)
    clashes = [b for b in T if len(b) >= 2]
    if len(clashes) == 0:
        print("No clashes!")
        # return T
    else:
        print("Clashes found.")
        return clashes


H = Hasher(MetaIndex.keys(), 5.0, 0.8)
print(checkPerfectHasher(MetaIndex.keys(), H))

# FOR INTEREST ONLY:

# Calculating 'essential size' of a Hasher, given a crude compression scheme
# (compression itself not implemented):

import math


def compressedSizeOf(H, bitWidth, maxOutlierSize):
    cutoff = 2 ** bitWidth - 1
    outliers = len([j for j in H.hashChoices if j >= cutoff])
    intermedKeySize = math.ceil(math.log2(H.r))
    return (((H.r - outliers) * bitWidth) +
            (outliers * (maxOutlierSize + intermedKeySize)))


def bestCompression(H):
    maxOutlierSize = math.ceil(math.log2(max(H.hashChoices)))
    comprList = [(i, compressedSizeOf(H, i, maxOutlierSize))
                 for i in range(3, maxOutlierSize)]
    best = comprList[0]
    for i in range(1, len(comprList)):
        if comprList[i][1] < best[1]:
            best = comprList[i]
    return {'bestBitWidth': best[0],
            'totalBitSize': best[1],
            'bitsPerKey': best[1] / H.n}

# End of file
