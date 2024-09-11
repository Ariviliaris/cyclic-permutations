import timeit
from prototypes import Permutation, Cycle, CycleDecomposition

def main():
    # c1 = Cycle([1, 3, 4, 5])
    # c2 = Cycle([6, 7])
    
    # new_c = c1 * c2

    # print(new_c.sign())
    # print(new_c.order())
    # print(new_c * new_c * new_c * new_c)
    # perm = Cycle([1, 3, 4, 5]).to_permutation()
    # print((perm.power(3)).to_cycles())
    # s1 = Cycle([2, 5]) * Cycle([1, 2, 3, 4])
    # print(s1 == s1.power(1))
    # print(s1.power(2))
    # print(s1.to_permutation().power(2).to_cycles())

    # print(sigma.to_cycles())
    # print(sigma.__repr__())
    # print(sigma.__str__())
    # print(sigma * sigma * sigma == sigma)
    # print((tau * tau.inverse()).to_cycles())
    # print(sigma)
    # print(sigma * tau)

    # execution_time = timeit.timeit(sigma * tau, number=1000)
    # print(f"Execution time for 1000 compositions: {execution_time:.6f} seconds")

    # perm = Permutation({1:2, 2:3, 3:4, 4:1, 5:5})
    # print(perm.to_cycles(include_one_cycles=True))

    # print(sigma)
    # print(sigma.to_cycles())
    def compose_cycles():
        s1 = Cycle([1, 2, 3, 4, 5, 6, 7]) * Cycle([2, 3, 4]) * Cycle([5]) * Cycle([5, 10, 7]) * Cycle([5, 7, 8]) * Cycle([3, 1, 5, 9, 8, 11, 12, 14, 13])
        s2 = Cycle([3, 2, 5, 8, 1]) * Cycle([4, 3, 1, 2]) * Cycle([11, 10, 9]) * Cycle([1, 4]) * Cycle([8, 2, 7, 5, 9, 1, 4, 3, 6, 10]) * Cycle([14, 13, 12])
        result = s1 * s2

    n = 1000
    execution_time = timeit.timeit(compose_cycles, number=n)
    print(f"Time taken to compose the cycles: {execution_time/n:.8f} seconds")

if __name__ == "__main__":
    main()