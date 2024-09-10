import timeit
from prototypes import Permutation, Cycle, CycleDecomposition

def main():
    c1 = Cycle([1, 3, 4, 5])
    c2 = Cycle([6, 7])
    
    new_c = c1 * c2

    print(new_c.sign())
    print(new_c.order())
    print(new_c * new_c * new_c * new_c)


    # print(sigma.to_cycles())
    # print(sigma.__repr__())
    # print(sigma.__str__())
    # print(sigma * sigma * sigma == sigma)
    # print((tau * tau.inverse()).to_cycles())
    # print(sigma)
    # print(sigma * tau)

    # execution_time = timeit.timeit(sigma * tau, number=1000)
    # print(f"Execution time for 1000 compositions: {execution_time:.6f} seconds")

# print(sigma)
# print(sigma.to_cycles())

if __name__ == "__main__":
    main()