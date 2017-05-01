from os import path
from PIL import Image
import numpy as np
import matplotlib.pyplot as plt
from nltk.corpus import stopwords
from wordcloud import WordCloud
from scipy.misc import imread

stop = stopwords.words("english")

def make(img_link):
    
    with open('commentary.txt', "r") as f:
        words = f.read()

    mask = imread(img_link, flatten=True)

    wc = WordCloud(background_color="white", max_words=2000, mask=mask, stopwords=stop)

    # generate word cloud
    wc.generate(words)

    # store to file
    wc.to_file(path.join(".", "tennis_2.png"))

    # show
    # plt.imshow(wc)
    # plt.axis("off")
    # plt.show()

if __name__ == '__main__':
    make("./tennis_2.jpg")