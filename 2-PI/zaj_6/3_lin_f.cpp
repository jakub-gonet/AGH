#include <stdio.h>

#include <iostream>
#include <list>
using namespace std;

void dfs(int u, list<int> *&graph, list<int> *&lines, int &added,
         bool &has_even_v) {
  while (!graph[u].empty()) {
    int v = graph[u].front();
    graph[v].remove(u);
    graph[u].pop_front();
    dfs(v, graph, lines, added, has_even_v);
  }

  if (!has_even_v && u == 0) {
    added++;
  } else {
    lines[added].push_back(u);
  }
}

int main() {
  int z;
  cin >> z;
  while (z--) {
    int n, m, u, v;
    cin >> n >> m;

    list<int> *graph = new list<int>[n + 1];
    list<int> *lines = new list<int>[n + 1];

    for (int i = 0; i < m; i++) {
      cin >> u >> v;
      graph[u].push_back(v);
      graph[v].push_back(u);
    }

    bool has_even_v = true;
    for (int i = 1; i <= n; i++)
      if (graph[i].size() % 2 != 0) {
        // helper vertex
        graph[i].push_back(0);
        graph[0].push_back(i);
        has_even_v = false;
      }

    int added = 0;
    // if we need helper, we use it
    if (has_even_v) {
      dfs(1, graph, lines, added, has_even_v);
    } else {
      dfs(0, graph, lines, added, has_even_v);
    }

    int lines_number = 0;

    for (int i = 0; i <= added; i++) {
      // counting how much we added
      if (lines[i].size() > 1) {
        lines_number++;
      }
    }

    cout << lines_number << endl;

    for (int i = 0; i <= lines_number; i++) {
      if (lines[i].size() > 1) {
        cout << lines[i].size() << " ";
        for (list<int>::iterator it = lines[i].begin(); it != lines[i].end();
             it++) {
          cout << *it << " ";
        }
        cout << endl;
      }
    }

    delete[] graph;
    delete[] lines;
  }
}
