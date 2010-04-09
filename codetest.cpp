#include <iostream>

using namespace std;

char str[] = "\"werrwrew\" werewsdf";

int main() {
  int pos = 1, len = 0;
  while(str[pos + len] && (str[pos + len] != '"'))
    len++;

  cout << "length is " << len;
  return 0;
}
