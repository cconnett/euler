#include <iostream>
#include <map>

using namespace std;

double par(double a, double b) {
  return a+b;
}
double seq(double a, double b) {
  return 1.0/(1.0/a + 1.0/b);
}

typedef map<double, int> mdi;

int main() {
  mdi newc;
  mdi c;

  c.insert(make_pair(60.0, 1));
  
  while (true) {
    for (mdi::iterator i = c.begin(); i != c.end(); i++) {
      for (mdi::iterator j = c.begin(); j != c.end(); j++) {
        //if (!(i->first <= j->first)) {
        //  continue;
        //}
        if (i->second + j->second <= 10) {
          //cout << i->first << " " << i->second << " "
          //     << j->first << " " << j->second << endl;
            
          double v = par(i->first, j->first);
          if (!c.count(v)) {
            if (newc.count(v)) {
              newc[v] = min(newc[v], i->second + j->second);
            } else {
              newc[v] = i->second + j->second;
            }
            //cout << newc[v] << endl;
          }

          /*double*/ v = seq(i->first, j->first);
          if (!c.count(v)) {
            if (newc.count(v)) {
              newc[v] = min(newc[v], i->second + j->second);
            } else {
              newc[v] = i->second + j->second;
            }
          }
        }
      }
    }
    //cout << "---" << endl;
    if (newc.empty()) {
      break;
    }
    for (mdi::iterator n = newc.begin(); n != newc.end(); n++) {
      if (c.count(n->first)) {
        c[n->first] = min(c[n->first], n->second);
      } else {
        c[n->first] = n->second;
      }
    }
    newc.clear();
  }
  
  cout << c.size() << endl;
  return 0;
}
