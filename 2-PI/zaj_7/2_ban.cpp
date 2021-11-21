#include <climits>
#include <iostream>
#include <vector>

using namespace std;
typedef struct edge {
  long w;
  int exists;
} edge;

enum possible { YES, NO, CYCLE };

pair<possible, int> bellman_ford(vector<vector<edge>> &adj_matrix, int start,
                                 int end) {
  int N = adj_matrix.size();

  vector<long> cost(N);
  for (vector<long>::size_type i = 0; i < cost.size(); i++) {
    cost[i] = LONG_MAX;
  }
  cost[start] = 0;

  for (int i = 0; i < N - 1; i++) {
    for (int u = 0; u < N; u++) {
      for (int v = 0; v < N; v++) {
        if (adj_matrix[u][v].exists) {
          if (cost[u] == LONG_MAX) {
            continue;
          }
          long w = adj_matrix[u][v].w;
          if (cost[u] + w < cost[v]) {
            cost[v] = cost[u] + w;
          }
        }
      }
    }
  }
  possible state = possible::YES;

  for (int u = 0; u < N; u++) {
    for (int v = 0; v < N; v++) {
      if (adj_matrix[u][v].exists) {
        if (cost[v] == LONG_MAX) {
          continue;
        }
        int w = adj_matrix[u][v].w;
        if (cost[v] > cost[u] + w) {
          state = possible::CYCLE;
        }
      }
    }
  }

  long total_cost = cost[end];
  if (total_cost == LONG_MAX) {
    state = possible::NO;
  }

  return pair<possible, int>(state, total_cost);
}

int main() {
  int n, k, start_b, end_b;
  cin >> n >> k >> start_b >> end_b;

  vector<vector<edge>> adj_matrix(n);
  for (int i = 0; i < n; i++) {
    adj_matrix[i] = vector<edge>(n);
    for (int j = 0; j < n; j++) {
      edge e;
      e.exists = false;
      adj_matrix[i][j] = e;
    }
  }

  while (k--) {
    int start, end, cost;
    cin >> start >> end >> cost;
    adj_matrix[start][end].w = cost;
    adj_matrix[start][end].exists = true;
  }

  pair<possible, int> res = bellman_ford(adj_matrix, start_b, end_b);

  if (res.first == possible::CYCLE) {
    cout << "CYCLE";
  } else if (res.first == possible::NO) {
    cout << "NO";
  } else {
    cout << res.second;
  }
}