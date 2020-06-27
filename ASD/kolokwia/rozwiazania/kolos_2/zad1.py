# Jakub Gonet

class Node:
    def __init__( self ):
        self.children = 0
        self.child = [] # lista par (dziecko, waga krawędzi)
        self.parent = None
        self.first_pass_acc = 0
        self.second_pass_acc = 0
        self.visited = False

def heavy_path(T):
    """
    Ignorujemy pole children, możemy użyć len()

    Najdłuższą ścieżkę możemy znaleźć przez połączenie jakichś dwóch ścieżek w dwóch poddrzewach, zakładając, że ich suma jest większa niż one osobno

    1. Przechodzimy po grafie akumulując wagi - koszt O(E+V)
    2. Znajdujemy wierzchołek z największą wagą
    3. Przechodzimy w drugą stronę po całym grafie akumulując wagi - DFS(V+E)
    4. Po przejściu grafu zwracamy największą wagę
    """
    def traverse(T, w, max_c, max_w):
        if(T.first_pass_acc > max_c.first_pass_acc):
            max_c = T
            max_w = w


        for (child, w) in T.child:
           child.parent = T
           child.first_pass_acc = T.first_pass_acc + w
           traverse(child, w, max_c, max_w)

        return (max_c, max_w)

    def find_longest_path(T, max_c, max_w):
        if T.second_pass_acc > max_c.second_pass_acc:
            max_w = T.second_pass_acc

        T.visited = True
        to_check = T.child
        if T.parent:
            to_check += [(T.parent, 0)]

        for (child, w) in to_check:
            if not child.visited:
                child.second_pass_acc += T.second_pass_acc + w
                find_longest_path(T, max_c, max_w)
        return max_w


    max_c, max_w = traverse(T, 0, T, 0)
    return find_longest_path(max_c, max_c, max_w)

A = Node()
B = Node()
C = Node()
A.children = 2
A.child = [ (B,5), (C,-1) ]

print(heavy_path(A))
