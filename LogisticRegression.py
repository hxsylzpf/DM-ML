import math
import numpy as np
from sklearn.datasets import load_iris

class LogisticRegression:
    
    def __init__(self):
        self.datas = self.load_data()
        self.weights = np.array([1.0]*len(self.datas[0][0]))
        self.learning_rate = 0.3
    
    def load_data(self):
        iris = load_iris()
        return [(feature,label) for feature,label in zip(iris.data,iris.target) if label!=2]
        
    def sigmod(self, w, x):
        return round(1.0 / (1.0 + math.e ** (-np.dot(w,x))),4)
    
    def train(self):
        weights_old = np.array(list(self.weights))
        while True:
            for data in self.datas:
                x = data[0]
                y = data[1]
                for i in range(len(x)):
                    delta = y - self.sigmod(self.weights, x)
                    self.weights[i] += round(delta * x[i] * self.learning_rate,4)
            if sum([abs(x) for x in self.weights-weights_old]) < 0.05:
                break
            weights_old = np.array(list(self.weights))
    
    def predict(self):
        for data in self.datas:
            x = data[0]
            y = data[1]
            if np.dot(self.weights,x) > 0.5:
                predict = 1
            else:
                predict = 0
            print 'predict: ', predict, 'real:',y
    
if __name__ == '__main__':
    lr = LogisticRegression()
    lr.train()
    lr.predict()