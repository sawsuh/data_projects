import ast
import itertools
import math
import numpy as np
import pandas as pd

total_possibilities = [''.join([str(j) for j in i]) for i in itertools.product(*[[0,1,2] for _ in range(5)])]
# 0 = grey, 1 = green, 2 = yellow
class Guess:
    def __init__(self, guess, acc='00000'):
        self.guess = guess
        self.acc = acc

    def get_acc(self):
        self.acc = input('accuracy: ')

    def set_acc(self, a):
        self.acc = a

    def compare(self, answer):
        acc = [0 for _ in range(5)]
        hinted = {l:0 for l in answer}
        for i in range(5):
            if self.guess[i] == answer[i]:
                acc[i] = 1
                hinted[answer[i]] += 1
                continue
        for i in range(5):
            if acc[i] == 0:
                remaining = [answer[k] for k in range(5) if acc[k] != 1]
                if self.guess[i] in remaining and remaining.count(self.guess[i]) > hinted[self.guess[i]]:
                    acc[i] = 2
                    hinted[self.guess[i]] += 1
        return ''.join([str(i) for i in acc])

    def acc_from(self, answer):
        self.acc = self.compare(answer)

    def check(self, answer):
        return self.acc == self.compare(answer)


# compute frequencies
class Game:
    def __init__(self, guessnum=0, guesses=[], wordspace=set(), answer_spce=set()):
        self.guessnum = guessnum
        self.guesses = guesses
        self.answer_spce = answer_spce
        self.wordspace = wordspace

    def restrict_aspace(self):
        old_len = len(self.answer_spce)
        new_spce = set()
        for a in self.answer_spce:
            if self.guesses[-1].check(a):
                new_spce.add(a)
                continue
        self.answer_spce = new_spce
        new_len = len(self.answer_spce)
        print(f'answer space: {old_len} ({math.log2(old_len):.3f} bits) -> {new_len} ({math.log2(new_len):.3f} bits), reduction of {old_len/new_len:.3f} or {math.log2(old_len/new_len):.3f} bits')

    def make_guess(self, guess):
        print(f'guessed {guess.guess}')
        if self.guessnum == 5:
            print('finished')
            return 0
        self.guessnum += 1
        guess.get_acc()
        print(f'guess={guess.guess}, acc={guess.acc}')
        self.guesses.append(guess)
        self.restrict_aspace()

    def recommend_guess(self):
        # recommend a guess given the most recent guesses, accuracies, answer space, and word space
        curr_H = float('inf')
        curr_guess = Guess('ERROR')
        for possible_guess in (Guess(i) for i in self.wordspace):
            if possible_guess in self.guesses:
                continue

            # calculate distribution of possible outcomes
            outcomes = pd.DataFrame({'outcome':total_possibilities})

            answer_df = pd.DataFrame({'answer': list(self.answer_spce)})
            answer_df['outcome'] = answer_df.apply(lambda r: possible_guess.compare(r['answer']), axis=1)

            outcomes = outcomes.merge(answer_df, on='outcome') \
                    .groupby('outcome') \
                    .size()
            outcomes = outcomes/outcomes.sum()

            # compute entropy and base decision on that
            E = (-1*outcomes * np.log2(outcomes)).sum()

            p_correct = (1/len(self.answer_spce)) if (possible_guess.guess in self.answer_spce) else 0
            expected_remaining_given_E = float('inf') if E<=0 else 1 + math.log2(len(self.answer_spce))/E
            H = p_correct + (1-p_correct)*expected_remaining_given_E
            if H < curr_H:
                print(f'current guess: {possible_guess.guess} with E={E:.3f}, p={p_correct:.3f} and H={H:.3f}', end='\r')
                curr_guess = possible_guess
                curr_H = H
        #return curr_guess
        print(f'\nguess: {curr_guess.guess}')
        return curr_guess.guess

    def play(self):
        self.make_guess(Guess('crane'))
        for i in range(5):
            if len(self.answer_spce) <= 1:
                print(self.answer_spce)
                break
            recommendation = self.recommend_guess()
            self.make_guess(Guess(input('guessed: ') or recommendation))


# read in words
with open('allowed.txt') as file:
#with open('answer_poss.txt') as file:
    allowed = {x for x in file.read().splitlines()}
with open('answer_poss.txt') as file:
    words = {x for x in file.read().splitlines()}
allowed = allowed.union(words)

main = Game(guessnum=0, guesses=[], wordspace=allowed, answer_spce=words)
main.play()
