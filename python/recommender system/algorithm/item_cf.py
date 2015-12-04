from collections import defaultdict
import util.similarity_calculator as sim_calculator


# build the structure of iter to users
def build_inverse_table(train):
    item_users = defaultdict(set)
    for user_id, items in train.items():
        for item in items:
            item_users[item].add(user_id)
    return item_users


def cal_item_item_sim(item_users):
    """
    calculate similarity between items
    :param item_users:
    :return: item similarity dict
    """
    item_item_sim = defaultdict(dict)
    for item_i, users_i in item_users.items():
        for item_j, users_j in item_users.items():
            if item_i == item_j:
                continue
            sim = sim_calculator.jaccard_sim(users_i, users_j)
            item_item_sim[item_i][item_j] = sim
            item_item_sim[item_j][item_i] = sim
    return item_item_sim


def recommend(users, train, k):
    """
    :param users: test users
    :param train: train data set
    :param k: top k most similar item
    :return: user_recommendation dict
    """
    # build inverse table for item users
    item_users = build_inverse_table(train)
    
    # calculate similarity between items
    item_item_sim = cal_item_item_sim(item_users)
            
    user_recommend = dict()
    for user_id in users:
        recommend = set()
        item_rating = defaultdict(float)
        for item_i_id in train[user_id]:
            for item_j in sorted(item_item_sim[item_i_id].items(), key=lambda item: item[1], reverse=True):
                if item_j[0] in train[user_id]:
                    continue
                sim = item_j[1]
                item_rating[item_j[0]] += sim
        for item_j in sorted(item_rating.items(), key=lambda item: item[1], reverse=True)[:k]:
            recommend.add(item_j[0])
            
        user_recommend[user_id] = recommend
        
    return user_recommend
