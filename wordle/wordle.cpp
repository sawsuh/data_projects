#include <fstream>
#include <iostream>
#include <map>
#include <math.h>
#include <omp.h>
#include <set>
#include <time.h>
#include <vector>
using namespace std;

#pragma omp declare reduction(setAdd : set<string> : omp_out.merge(omp_in))    \
    initializer(omp_priv = omp_orig)

class Guess {
public:
  string guess, acc;
  bool operator==(Guess other) {
    return ((guess == other.guess) && (acc == other.acc));
  }
  void get_acc() {
    cout << "accuracy: ";
    cin >> acc;
  }
  void get_acc_from(string a) { acc = this->compare(a); }
  void set_acc(string a) { acc = a; }
  string compare(string answer) {
    string cur_acc = "00000";
    map<char, int> hinted;
    for (int i = 0; i < 5; i++) {
      hinted[answer[i]] = 0;
    }
    for (int i = 0; i < 5; i++) {
      if (guess[i] == answer[i]) {
        cur_acc[i] = '1';
        hinted[answer[i]]++;
      }
    }
    for (int i = 0; i < 5; i++) {
      if (cur_acc[i] == '0') {
        // vector<char> remaining;
        map<char, int> remaining;
        for (int j = 0; j < 5; j++) {
          if (cur_acc[j] != '1') {
            // remaining.push_back(answer[j])
            if (remaining.find(answer[j]) == remaining.end()) {
              remaining[answer[j]] = 1;
            } else {
              remaining[answer[j]]++;
            }
          }
        }
        // letter_in_remaining = find(remaining.begin(), remaining.end(),
        // guess[i]) != remaining.end()
        bool letter_in_remaining = remaining.find(guess[i]) != remaining.end();
        if (letter_in_remaining && (remaining[guess[i]] > hinted[guess[i]])) {
          cur_acc[i] = '2';
          hinted[guess[i]]++;
        }
      }
    }
    return cur_acc;
  }
  bool check(string answer) { return (this->compare(answer) == acc); }
};

struct guess_p_h {
  Guess g;
  double p, h;
};

guess_p_h get_better(guess_p_h left, guess_p_h right) {
  return ((right.h < left.h) || ((right.h == left.h) && (right.p > left.p)))
             ? right
             : left;
}

#pragma omp declare reduction(setMax:guess_p_h : omp_out =                     \
                                  get_better(omp_in, omp_out))                 \
    initializer(omp_priv = omp_orig)

class Game {
public:
  int guessnum;
  vector<Guess> guesses;
  set<string> answer_spce, word_spce, total_possibilities;
  string answer;
  bool self_play, finished;
  Guess recommendation;
  ofstream outfile;
  double recent_entropy, recent_p;
  Game(bool s) {

    // initialise logging
    outfile.open("log.csv", ios_base::app);

    finished = false;

    guessnum = 0;

    self_play = s;

    // initialise answer space
    string line;
    ifstream file("answer_poss.txt");
    while (getline(file, line)) {
      answer_spce.insert(line);
    }
    file.close();

    // initialise word space
    file.open("allowed.txt");
    while (getline(file, line)) {
      word_spce.insert(line);
    }
    file.close();
    for (auto i : answer_spce) {
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
    set<string> tp{"0", "1", "2"};
    for (int i = 1; i < 5; i++) {
      set<string> output;
      for (auto j : tp) {
        output.insert(j + "0");
        output.insert(j + "1");
        output.insert(j + "2");
      }
      tp = output;
    }
    total_possibilities = tp;
  }
  void restrict_aspce() {
    int old_len = answer_spce.size();
    set<string> new_spce;
#pragma omp parallel reduction(setAdd : new_spce)
    for (auto it : answer_spce) {
      if (guesses.back().check(it)) {
        new_spce.insert(it);
      }
    }
    answer_spce = new_spce;
    int new_len = answer_spce.size();
    cout << "answer space: " << old_len << " (" << log2(old_len) << " bits) -> "
         << new_len << " (" << log2(new_len) << " bits)" << endl;
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
    Guess curr_guess;
    curr_guess.guess = "ERROR";
    guess_p_h cur{curr_guess, 0, 1000};
#pragma omp parallel reduction(setMax : cur)
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
      map<string, double> outcomes;
      double answer_size = answer_spce.size();
      for (auto answer : answer_spce) {
        string comparison = possible_guess.compare(answer);
        if (outcomes.find(comparison) == outcomes.end()) {
          outcomes[comparison] = 1 / answer_size;
        } else {
          outcomes[comparison] += 1 / answer_size;
        }
      }
      // compute entropy based on distribution
      double E = 0;
      for (auto const &x : outcomes) {
        E -= x.second * log2(x.second);
      }

      double p_correct = 0;
      if (answer_spce.find(possible_guess.guess) != answer_spce.end()) {
        p_correct = 1 / answer_size;
      }
      // Compute expected number of future turns using model
      //
      // double H = (1 - p_correct) * ((E>0)?(log2(answer_size)/E):100);
      // double H = E;
      double H = 1.0832 - 0.1352 * (this->guessnum + 1) +
                 0.3895 * log2(this->answer_spce.size()) - 0.5815 * p_correct -
                 0.1797 * E;

      guess_p_h candidate_guess{possible_guess, p_correct, H};
      cur = get_better(cur, candidate_guess);
    }
    recent_entropy = cur.h;
    recent_p = cur.p;
    cout << "current guess: " << cur.g.guess << ", p=" << cur.p
         << ", H=" << cur.h << endl;
    recommendation = cur.g;
  }
  void play() {
    Guess initial;
    if (self_play) {
      initial.guess = "soare";
    } else {
      cin >> initial.guess;
    }
    this->make_guess(initial);
    bool init = true;
    int old_asp_size, new_asp_size;
    for (int i = 0; i < 5; i++) {
      if (init) {
        init = false;
      } else if (self_play) {
        outfile << guessnum << ", " << guesses.back().guess << ", "
                << recent_entropy << ", " << old_asp_size << ", "
                << new_asp_size << ", " << recent_p << ", " << answer << endl;
      }
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
        if (next.guess.size() < 5) {
          next = recommendation;
        }
      }
      old_asp_size = answer_spce.size();
      this->make_guess(next);
      new_asp_size = answer_spce.size();
    }
  }
};

int main() {
  int sum = 0;
  int count = 0;
  Game blank(false);
  // blank.play();
  bool starting = true;
  for (auto i : blank.answer_spce) {
    if (starting) {
      if (i == "donor") {
        starting = false;
      }
      continue;
    }
    Game g("true");
    g.answer = i;
    g.play();
    count++;
    sum += g.guessnum;
    cout << "\n recent: " << g.guessnum << endl
         << "count: " << count << endl
         << "average: " << (double)sum / (double)count << endl
         << endl;
  }
}
