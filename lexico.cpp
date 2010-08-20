#include <vector>
#include <algorithm>
#include <iostream>

using namespace std;

int main() {
  vector<int> v;
  v.push_back(0);
  v.push_back(1);
  v.push_back(2);
  v.push_back(3);
  v.push_back(4);
  v.push_back(5);
  v.push_back(6);
  v.push_back(7);
  v.push_back(8);
  v.push_back(9);

  for (int i = 0; i < 999999; i++) {
    next_permutation(v.begin(), v.end());
  }

  for (int i = 0; i < v.size(); i++) {
    cout << v[i];
  }
  cout << endl;
}
