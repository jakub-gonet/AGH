#include <climits>
#include <iostream>
#include <queue>
#include <vector>

using namespace std;

int pop_min(vector<int> &Q, vector<int> &dist) {
  int min = 0;
  for (int i = 0; i < Q.size(); i++) {
    if (dist[Q[i]] < dist[Q[min]]) {
      min = i;
    }
  }
  int min_e = Q[min];
  Q.erase(Q.begin() + min);
  return min_e;
}

int dijkstra(vector<vector<int>> &adj_matrix, int start, int end) {
  if (start == end) {
    return 0;
  }
  // prepare dist array
  vector<int> dist(adj_matrix.size());
  for (int i = 0; i < dist.size(); i++) {
    dist[i] = INT_MAX;
  }
  dist[start] = 0;
  vector<int> Q(adj_matrix.size());

  for (int i = 0; i < adj_matrix.size(); i++) {
    Q[i] = i;
  }

  while (Q.size() > 0) {
    int u = pop_min(Q, dist);
    if (u == end) {
      return dist[u];
    }
    for (int v = 0; v < adj_matrix[u].size(); v++) {
      int w = adj_matrix[u][v];
      if (w == -1) {
        continue;
      }
      if (dist[u] + w < dist[v]) {
        dist[v] = dist[u] + w;
      }
    }
  }
  return -1;
}

int main() {
  int n, k, path_start, path_end;
  cin >> n >> k >> path_start >> path_end;

  vector<vector<int>> adj_matrix(n);
  for (int i = 0; i < n; i++) {
    adj_matrix[i] = (vector<int>(n, -1));
  }

  while (k--) {
    int start, end, cost;
    cin >> start >> end >> cost;
    adj_matrix[start][end] = cost;
    adj_matrix[end][start] = cost;
  }

  int min_dist = dijkstra(adj_matrix, path_start, path_end);

  cout << min_dist << endl;
}