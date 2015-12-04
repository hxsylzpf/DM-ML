def recall(recommendations, train):
    hit = 0
    total = 0
    for user_ID,recommend in recommendations.items():
        hit += len(train[user_ID]&recommend)
        total += len(train[user_ID])
    return float(hit)/total


def precision(recommendations, train):
    hit = 0
    total = 0
    for user_ID, recommend in recommendations.items():
        hit += len(train[user_ID] & recommend)
        total += len(recommend)
    return float(hit) / total
