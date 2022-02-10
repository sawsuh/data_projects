#include <iostream>
#include <fstream>
#include <map>
#include <set>
#include <vector>
#include <math.h>
#include <time.h>
using namespace std;

class Guess {
    public:
      string guess, acc;
    bool operator==(Guess other) {
      if ((guess == other.guess) && (acc == other.acc)) {
        return true;
      }
      return false;
    }
    void get_acc() {
      cout << "accuracy: ";
      cin >> acc;
    }
    void get_acc_from(string a) {
      acc = this->compare(a);
    }
    void set_acc(string a) {acc=a;}
    string compare(string answer) {
      string cur_acc = "00000";
      map<char, int> hinted;
      for (int i=0; i<5; i++) {
        hinted[answer[i]] = 0;
      }
      for (int i=0; i<5; i++) {
        if (guess[i] == answer[i]) {
          cur_acc[i] = '1';
          hinted[answer[i]]++;
        }
      }
      for (int i=0; i<5; i++) {
        if (cur_acc[i] == '0') {
          //vector<char> remaining;
          map<char, int> remaining;
          for (int j=0; j<5; j++) {
            if (cur_acc[j] != '1') {
              //remaining.push_back(answer[j])
              if (remaining.find(answer[j]) == remaining.end()) {
                remaining[answer[j]] = 1;
              } else {
                remaining[answer[j]]++;
              }
            }
          }
          //letter_in_remaining = find(remaining.begin(), remaining.end(), guess[i]) != remaining.end()
          bool letter_in_remaining = remaining.find(guess[i]) != remaining.end();
          if (letter_in_remaining && remaining[guess[i]] > hinted[guess[i]]) {
            cur_acc[i] = '2';
            hinted[guess[i]]++;
          }
        }
      }
      return cur_acc;
    }
    bool check(string answer) {
      return (this->compare(answer) == acc);
    }
};

class Game {
  public:
    int guessnum;
    vector<Guess> guesses;
    set<string> answer_spce;
    set<string> word_spce;
    set<string> total_possibilities;
    string answer;
    bool self_play;
    Guess recommendation;
    bool finished;
  Game(bool s) {
    finished = false;

    guessnum = 0;

    self_play = s;

    // initialise answer space
    string line;
    ifstream file("answer_poss.txt");
    while(getline(file, line)) {
      answer_spce.insert(line);
    }
    file.close();

    // initialise word space
    file.open("allowed.txt");
    while(getline(file, line)) {
      word_spce.insert(line);
    }
    file.close();
    for (auto i: answer_spce) {
      if (word_spce.find(i) == word_spce.end()) {
        word_spce.insert(i);
      }
    }

    if (self_play) {
      srand(time(NULL));
      auto r = rand() % answer_spce.size();
      auto it = begin(answer_spce);
      advance(it, r);
      answer = *it;
    }

    // initialise possibilities
    set<string> tp;
    tp.insert("0");
    tp.insert("1");
    tp.insert("2");
    for (int i=1; i<5; i++) {
      set<string> output;
      for (auto j : tp) {
        output.insert(j+"0");
        output.insert(j+"1");
        output.insert(j+"2");
      }
      tp = output;
    }
    total_possibilities = tp;
  }
  void set_answer(string a) {
    answer = a;
  }
  void restrict_aspce() {
    int old_len = answer_spce.size();
    set<string> new_spce;
    //for (string* i=answer_spce.begin(); i != answer_spce.end(); i++) {
    for (auto i : answer_spce) {
      if (guesses.back().check(i)) {
        new_spce.insert(i);
      }
    }
    answer_spce = new_spce;
    int new_len = answer_spce.size();
    cout << "answer space: " << old_len << " (" << log2(old_len) << " bits) -> " <<
      new_len << " (" << log2(new_len) << " bits)" << endl;
  }
  void make_guess(Guess guess) {
    cout << "guessed " << guess.guess << endl;
    guessnum++;
    if (self_play) {
      guess.get_acc_from(answer);
    } else {
      guess.get_acc();
    }
    cout << "guess=" << guess.guess << ", acc=" << guess.acc << endl;
    guesses.push_back(guess);
    if (guess.guess == answer) {
      finished = true;
    }
    this->restrict_aspce();
  }
  void recommend_guess() {
    if (answer_spce.size() == 1) {
      recommendation.guess = *(answer_spce.begin());
      return;
    }
    //float curr_H = 100;
    float curr_H = -1;
    Guess curr_guess;
    curr_guess.guess = "ERROR";
    float curr_p = 0;
    for (auto i : word_spce) {
      Guess possible_guess;
      possible_guess.guess = i;
      bool found = false;
      for (auto g : guesses) {
        if (g.guess == possible_guess.guess)
          found = true;
          break;
      }
      if (found) {
        continue;
      }

      // calculate distribution of possible outcomes
      map<string, float> outcomes;
      float answer_size = answer_spce.size();
      for (auto answer : answer_spce) {
        string comparison = possible_guess.compare(answer);
        if (outcomes.find(comparison) == outcomes.end()) {
          outcomes[comparison] = 1/answer_size;
        } else {
          outcomes[comparison] += 1/answer_size;
        }
      }
      // compute entropy based on distribution
      double E = 0;
      for (auto const& x : outcomes) {
        E -= x.second * log2(x.second);
      }

      double p_correct = 0;
      if (answer_spce.find(possible_guess.guess) != answer_spce.end()) {
        p_correct = 1/answer_size;
      }
      //double H = (1 - p_correct) * ((E>0)?(log2(answer_size)/E):100);
      double H = E;
      if ((H > curr_H) || ((H == curr_H) && (p_correct > curr_p))){
        curr_p = p_correct;
        curr_H = H;
        curr_guess = possible_guess;
        cout << "current guess: " <<
          curr_guess.guess << " with E=" <<
          E << ", p=" << curr_p <<
          ", H=" << H << endl;
      }
    }
    cout << "\n guess:" << curr_guess.guess << endl;
    //return curr_guess.guess;
    recommendation = curr_guess;
  }
  void play() {
    Guess initial;
    initial.guess = "soare";
    this->make_guess(initial);
    for (int i=0; i<5; i++) {
      if ((!self_play) && (answer_spce.size() <= 1)) {
        cout << *(answer_spce.begin()) << endl;
      }
      if (finished) {
        cout << "finished" << endl;
        return;
      }
      this->recommend_guess();
      Guess next;
      if (self_play) {
        next = recommendation;
      } else {
        cin >> next.guess;
        if (next.guess.size()<5) {
          next = recommendation;
        }
      }
      this->make_guess(next);
    }
  }
};

int main() {
  int sum = 0;
  int count = 0;
  Game blank(false);
  //for(int i=0; i<300; i++) {
  for(auto i: blank.answer_spce) {
    Game g(true);
    g.set_answer(i);
    g.play();
    count++;
    sum += g.guessnum;
    cout << "\n recent: " << g.guessnum << endl <<
      "count: " << count << endl <<
      "average: " << (float)sum/(float)count << endl << endl;
  }
}
