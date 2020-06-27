#include <iostream>
#include <list>
#include <unordered_set>
#include <vector>
using namespace std;

void dfs(vector<list<int>> &adj_list, list<int> &path, int v) {
  while (!adj_list[v].empty()) {
    // get next vertex
    int i = adj_list[v].front();
    // remove G_i_v and G_v_i
    adj_list[i].remove(v);
    adj_list[v].pop_front();
    dfs(adj_list, path, i);
    path.push_back(v);
  }

  // add vertex to path
  path.push_back(v);
}

int main(void) {
  int z;
  cin >> z;
  while (z--) {
    int v, e;
    cin >> v >> e;
    // create adj matrix
    vector<list<int>> adj_list;
    for (int i = 0; i < v; i++) {
      list<int> neigh;
      adj_list.push_back(neigh);
    }
    // fill edges
    for (int i = 0; i < e; i++) {
      int a, b;
      cin >> a >> b;
      a--;
      b--;
      adj_list[a].push_back(b);
      adj_list[b].push_back(a);
    }

    // dfs
    vector<list<int>> paths;
    for (int i = 0; i < v; i++) {
      list<int> path;
      dfs(adj_list, path, i);
      if (path.size() > 1) {
        paths.push_back(path);
      }
    }

    for (int i = 0; i < paths.size(); i++) {
      cout << paths[i].size() << " ";
      while (!paths[i].empty()) {
        int v = paths[i].front();
        paths[i].pop_front();
        cout << v + 1 << " ";
      }
      cout << endl;
    }
    // create paths
    vector<list<int>> fixed_paths;
    for (int i = 0; i < paths.size(); i++) {
      unordered_set<int> visited;
      list<int> new_path;

      while (!paths[i].empty()) {
        int v = paths[i].front();
        paths[i].pop_front();
        if (visited.count(v) == 1) {
          // found duplicate
          if (paths[i].empty()) {
            // last element
            new_path.push_back(v);
          }
          fixed_paths.push_back(list<int>(new_path));
          visited.clear();
          new_path.clear();
        } else {
          new_path.push_back(v);
          visited.insert(v);
        }
      }
      fixed_paths.push_back(list<int>(new_path));
    }
    paths = fixed_paths;

    for (auto path = paths.begin(); path != paths.end();) {
      if (path->size() == 0) {
        path = paths.erase(path);
      } else {
        ++path;
      }
    }

    int count = paths.size();
    cout << count << endl;
    for (int i = 0; i < count; i++) {
      cout << paths[i].size() << " ";
      while (!paths[i].empty()) {
        int v = paths[i].front();
        paths[i].pop_front();
        cout << v + 1 << " ";
      }
      cout << endl;
    }
  }
}