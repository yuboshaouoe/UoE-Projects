import time
import os
from index_build import MetaIndex, generateAllIndexEntries, generateMetaIndex, mergeFilesInRange, splitIntoSortedChunks, buildIndex
from perfect_hashing import Hasher, checkPerfectHasher
from search_queries import HitStream, allHits, makeItemStreams, ordinals, HitStreamQ

def timefunc(f, *args):
    start_time = time.time()
    res = f(*args)
    end_time = time.time()
    return (end_time - start_time) * 1000, res

def main():
    print("##############")
    def operation_slow(): return allHits(HitStream(makeItemStreams(ordinals), 10))
    t1, hits_slow = timefunc(operation_slow)
    print("allHitsSlow: {:.0f}ms".format(t1))
    def operation_fast(): return allHits(HitStreamQ(makeItemStreams(ordinals), 10))
    t2, hits_fast = timefunc(operation_fast)
    print("allHitsFast: {:.0f}ms".format(t2))
    print("##############")
    assert hits_slow == hits_fast
    print("assertion passed! HitStream and HitStreamQ are equivalent.")
    print(f"relative speedup: {t1 / t2:.02f}x")

if __name__ == "__main__":
    main()