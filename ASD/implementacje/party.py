class Employee:
    def __init__(self, fun, name):
        self.emp = []
        self.fun = fun
        self.name = name
        self.best_party = None
        self.best_party_wo_emp = None
        self.going_to_party = False


def best_party_wo_employee(employee):
    if employee.best_party_wo_emp is not None:
        return employee.best_party_wo_emp
    employee.best_party_wo_emp = sum(
        [best_party(subordinate) for subordinate in employee.emp]
    )
    return employee.best_party_wo_emp


def best_party(employee):
    if employee.best_party is not None:
        return employee.best_party

    going_to_party = employee.fun + sum(
        (best_party_wo_employee(subordinate) for subordinate in employee.emp)
    )
    not_going_to_party = best_party_wo_employee(employee)

    # FIXME
    if going_to_party > not_going_to_party:
        employee.going_to_party = True
        for subordinate in employee.emp:
            subordinate.going_to_party = False
    else:
        employee.going_to_party = False
    employee.best_party = max(going_to_party, not_going_to_party)
    return employee.best_party


def traverse(root):
    partiers = []

    def f(current):
        if current.going_to_party:
            partiers.append(current.name)
        for e in current.emp:
            f(e)

    f(root)
    return partiers


boss = Employee(20, "John")
boss.emp = [Employee(10, "James"), Employee(5, "Anna"), Employee(10, "Samantha")]
boss.emp[0].emp = [Employee(1, "Sebastian"), Employee(1, "Adam"), Employee(3, "Simone")]
boss.emp[1].emp = [Employee(2, "Sophia"), Employee(3, "Alex")]
boss.emp[2].emp = [Employee(15, "Bob")]

print(best_party(boss))
print(traverse(boss))
