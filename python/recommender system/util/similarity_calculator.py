import math


def cosine_sim(s1, s2):
    # s1 and s2 are both type of set
    return float(len(s1 & s2))/math.sqrt(len(s1)*len(s2))


def jaccard_sim(s1, s2):
    return float(len(s1 & s2))/len(s1.union(s2))
    
if __name__ == "__main__":
    s1 = set([1,2])
    s2 = set([1,3])
    print jaccard_sim(s1,s2)