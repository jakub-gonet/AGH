#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define SIZE 32
typedef struct node {
  int id;
  char data[SIZE];
  struct node* right;
  struct node* left;
} node;

node* new_node(int id, char* buf) {
  node* p = malloc(sizeof(struct node));
  p->id = id;
  strcpy(p->data, buf);
  p->left = NULL;
  p->right = NULL;

  return p;
}

node* insert(struct node* root, int id, char* buf) {
  if (root == NULL) {
    return new_node(id, buf);  ///
  } else if (id > root->id) {
    root->right = insert(root->right, id, buf);  ///
  } else {
    root->left = insert(root->left, id, buf);  ///
  }
  return root;
}

node* find(node* root, int id) {
  if (root == NULL || root->id == id) {
    return root;
  } else if (id > root->id) {
    return find(root->right, id);
  } else {
    return find(root->left, id);
  }
}

node* min_succ(struct node* node) {
  while (node && node->left != NULL) {
    node = node->left;
  }
  return node;
}

node* _remove(struct node* root, int id) {
  if (root == NULL) {
    return root;
  }

  if (id < root->id) {
    root->left = _remove(root->left, id);
  } else if (id > root->id) {
    root->right = _remove(root->right, id);
  } else {
    if (root->left == NULL) {
      struct node* tmp = root->right;
      free(root);
      return tmp;
    } else if (root->right == NULL) {
      struct node* tmp = root->left;
      free(root);
      return tmp;
    }

    node* succ = min_succ(root->right);
    root->id = succ->id;
    strcpy(root->data, succ->data);
    root->right = _remove(root->right, succ->id);
  }
  return root;
}

int main(void) {
  int a, r, f;
  int id;
  char buf[SIZE];
  node* bst = NULL;
  scanf("%d %d %d", &a, &r, &f);
  while (a--) {
    scanf("%d %s", &id, buf);
    bst = insert(bst, id, buf);
  }
  while (r--) {
    scanf("%d", &id);
    bst = _remove(bst, id);
  }
  while (f--) {
    scanf("%d", &id);
    node* n = find(bst, id);
    printf("%s\n", n != NULL ? n->data : "NO");
  }
}
