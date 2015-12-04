from collections import defaultdict
import util.similarity_calculator as sim_calculator


def get_similar_users(test_user_id, train, k):
    # get top k  most similar users
    user_similarity = dict()
    for train_user_id, movies_train in train.items():
        if train_user_id == test_user_id:
            continue
        user_similarity[train_user_id] = sim_calculator.cosine_sim(train[test_user_id], movies_train)
    user_similarity = sorted(user_similarity.items(), key=lambda item: item[1], reverse=True)[:k]
    return user_similarity


def recommend(test, train, k):
    # return user_recommendation dict
    user_recommend = dict()
    for user_test in test:
        movie_rating = defaultdict(float)
        sim_users = get_similar_users(user_test, train, k)
        for sim_user in sim_users:
            user_id = sim_user[0]
            similarity = sim_user[1]
            for movie in train[user_id]:
                movie_rating[movie] += similarity
                
        recommendation = set()
        for movie_id in train[user_test]:
            if movie_id in movie_rating:
                movie_rating.pop(movie_id)
                
        for item in sorted(movie_rating.items(), key=lambda item: item[1], reverse=True)[:k]:
            recommend.add(item[0])
        user_recommend[user_test] = recommend
    
    return user_recommend
