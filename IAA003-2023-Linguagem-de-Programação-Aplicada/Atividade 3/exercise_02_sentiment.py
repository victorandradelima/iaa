import sys
from sklearn.feature_extraction.text import TfidfVectorizer
from sklearn.svm import LinearSVC
from sklearn.pipeline import Pipeline
from sklearn.model_selection import GridSearchCV
from sklearn.datasets import load_files
from sklearn.model_selection import train_test_split
from sklearn import metrics


if __name__ == "__main__":
    # NOTE: we put the following in a 'if __name__ == "__main__"' protected
    # block to be able to use a multi-core grid search that also works under
    # Windows, see: http://docs.python.org/library/multiprocessing.html#windows
    # The multiprocessing module is used as the backend of joblib.Parallel
    # that is used when n_jobs != 1 in GridSearchCV

    # the training data folder must be passed as first argument
    movie_reviews_data_folder = r"./data"
    dataset = load_files(movie_reviews_data_folder, shuffle=False)
    print("n_samples: %d" % len(dataset.data))

    # split the dataset in training and test set:
    docs_train, docs_test, y_train, y_test = train_test_split(
        dataset.data, dataset.target, test_size=0.25, random_state=None)

    # TASK: Build a vectorizer / classifier pipeline that filters out tokens
    # that are too rare or too frequent
    from sklearn.feature_extraction.text import TfidfTransformer
    text_clf = Pipeline([('vect', TfidfVectorizer()),
                         ('tfidf', TfidfTransformer()),
                         ('clf', LinearSVC()),
    ])

    # TASK: Build a grid search to find out whether unigrams or bigrams are
    # more useful.
    # Fit the pipeline on the training set using grid search for the parameters
    parameters = {'vect__ngram_range': [(1, 1), (1, 2)]}
    gs_clf = GridSearchCV(text_clf, parameters, n_jobs=-1)
    gs_clf = gs_clf.fit(docs_train, y_train)
    
    # TASK: print the cross-validated scores for the each parameters set
    # explored by the grid 
    print("\n")
    print("Teste do GridSearchCV")
    for param_name in sorted(parameters.keys()):                
        count = 0
        for param in gs_clf.cv_results_['params']:
            print(param[param_name], ": ",gs_clf.cv_results_['mean_test_score'][count])
            count = count + 1
    print("Melhor resultado: ", param_name, " - ", gs_clf.best_params_[param_name], " - ", gs_clf.best_score_)

    # TASK: Predict the outcome on the testing set and store it in a variable
    # named y_predicted
    print("\n")
    print("Teste de predileção")
    import numpy as np
    y_predicted = gs_clf.predict(docs_test)
    print("Média de acerto:", np.mean(y_predicted == y_test))

    # Print the classification report
    print("\n")
    print("Métricas")
    print(metrics.classification_report(y_test, y_predicted, target_names=dataset.target_names))

    # Print and plot the confusion matrix
    print("\n")
    print("Matriz de confusão")
    cm = metrics.confusion_matrix(y_test, y_predicted)
    print(cm)

    import matplotlib.pyplot as plt
    plt.matshow(cm)
    plt.show()
