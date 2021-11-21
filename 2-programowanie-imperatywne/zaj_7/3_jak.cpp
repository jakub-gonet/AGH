#include <climits>
#include <iomanip>
#include <iostream>
#include <vector>
using namespace std;

void add_edge(vector<vector<int>> &adj_matrix, int from, int to, int time) {
  if (time < adj_matrix[from][to]) {
    adj_matrix[from][to] = time;
    adj_matrix[to][from] = time;
  }
}

void floyd_warshall(vector<vector<int>> &adj_matrix) {
  int n = adj_matrix.size();
  for (int k = 0; k < n; k++) {
    vector<vector<int>> old_matrix = adj_matrix;
    for (int i = 0; i < n; i++) {
      for (int j = 0; j < n; j++) {
        if (old_matrix[i][k] == INT_MAX or old_matrix[k][j] == INT_MAX) {
          continue;
        }

        if (old_matrix[i][k] + old_matrix[k][j] < old_matrix[i][j]) {
          adj_matrix[i][j] = old_matrix[i][k] + old_matrix[k][j];
        }
      }
    }
  }
}
int main() {
  int t, n, q;
  cin >> t >> n >> q;
  vector<vector<int>> adj_matrix(t);
  for (int i = 0; i < t; i++) {
    adj_matrix[i] = vector<int>(t);
    for (int j = 0; j < t; j++) {
      adj_matrix[i][j] = INT_MAX;
    }
  }

  while (n--) {
    int count, stop_t = -1;
    int stop = -1, prev_stop = -1;

    cin >> count;
    for (int i = 0; i < count - 1; i++) {
      prev_stop = stop;
      cin >> stop;
      if (prev_stop != -1) {
        add_edge(adj_matrix, prev_stop, stop, stop_t);
      }
      cin >> stop_t;
    }
    prev_stop = stop;
    cin >> stop;
    add_edge(adj_matrix, prev_stop, stop, stop_t);
  }

  floyd_warshall(adj_matrix);

  while (q--) {
    int a, b;
    cin >> a >> b;
    cout << adj_matrix[a][b] << endl;
  }
}