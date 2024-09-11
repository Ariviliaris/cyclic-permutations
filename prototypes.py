from math import gcd
from functools import reduce

class InvalidPermutationError(Exception):
    """Custom exception for invalid permutations (when there is no bijection)."""
    pass

class NonIntegerElementError(Exception):
    """Custom exception for non-integer elements in permutation."""
    pass

class InvalidCycleError(Exception):
    """Exception raised when a cycle is invalid."""
    pass

class InvalidCycleDecompositionError(Exception):
    """Exception raised when a cycle decomposition is invalid."""
    pass

class Permutation:
    def __init__(self, mapping, validated=False):
        """
        Initialise the permutation with a mapping.

        :param mapping: A dictionary where keys and values are integers representing the permutation.
        """
        self.permutation = mapping
        self.validated = validated
        if not validated:
            if not self._is_valid_permutation():
                raise InvalidPermutationError("Incorrect permutation: All elements must map to exactly one other element.")
            elif not self._are_integers():
                raise NonIntegerElementError("Incorrect permutation: All elements must be integer values.")
            self.validated = True

    def _is_valid_permutation(self):
        """
        Check if the permutation is valid (i.e., it's a bijection).
        :return: True if valid, otherwise False.
        """
        values = set(self.permutation.values())
        keys = set(self.permutation.keys())
        return keys == values
    
    def _are_integers(self):
        """ Check if all keys and values in the permutation are integers. """
        return all(isinstance(k, int) and isinstance(v, int) for k, v in self.permutation.items())
    
    def __repr__(self):
        return f"Permutation({self.permutation})"
    
    def display(self):
        """
        Sort the keys of the mappings dictionary, then output them and their mappings in a table.
        :return: a string representation of the permutation in a table format.
        """
        keys = sorted(self.permutation.keys())
        values = [self.permutation[k] for k in keys]
        table = "Permutation Table\n-----------------\n"
        table += "Element | " + " ".join(f"{k:2}" for k in keys) + "\n"
        table += "Maps to | " + " ".join(f"{v:2}" for v in values) + "\n"
        return table
    
    def __eq__(self, other):
        """
        Check if two permutations are equal.
        :param other: Another Permutation object.
        :return: True if the permutations are the same, False otherwise.
        """
        if isinstance(other, Permutation):
            return self.permutation == other.permutation
        return False

    def __mul__(self, other):
        """
        Define multiplication (using *) as composition of two permutations.
        This allows p1 * p2 to work the same as p1.compose(p2).
        """
        return self.compose(other)
    
    def __len__(self):
        """
        Return the number of elements in the permutation.
        :return: The number of keys (or values) in the permutation.
        """
        return len(self.permutation)
    
    def sign(self):
        """
        Calculate the sign of the permutation based on its cycle decomposition.
        :return: +1 if the permutation is even, -1 if odd.
        """
        cycle_decomposition = self.to_cycles()
        return cycle_decomposition.sign()
    
    def order(self):
        """
        Calculate the order of the permutation.
        The order is the least common multiple (LCM) of the orders of its cycles.
        :return: The order of the permutation.
        """
        cycle_decomposition = self.to_cycles()
        return cycle_decomposition.order()
    
    def inverse(self):
        """
        Return the inverse of the permutation.
        """
        inverse_mapping = {v: k for k, v in self.permutation.items()}
        return Permutation(inverse_mapping)

    def compose(self, other):
        """
        Compose this permutation with another permutation.
        """
        composed_mapping = {}
        all_keys = set(self.permutation.keys()).union(other.permutation.keys())

        for k in all_keys:
            first = other.permutation.get(k, k)
            composed_mapping[k] = self.permutation.get(first, first)

        return Permutation(composed_mapping)
    
    def power(self, n):
        """
        Raise the permutation to the power of n by composing it with itself n times.
        :param n: The exponent to which to raise the permutation (can be positive or negative).
        :return: A new Permutation object representing the result.
        """
        if n == 0:
            return Permutation({key: key for key in self.permutation})

        if n < 0:
            return self.inverse().power(-n)

        result = self
        for _ in range(n - 1):
            result = result * self

        return result

    def to_cycles(self):
        """
        Convert the permutation into its disjoint cycle form.
        :return: A CycleDecomposition object representing the disjoint cycles.
        """
        visited = set()
        cycles = []

        all_elements = set(self.permutation.keys()).union(set(self.permutation.values()))

        for start in all_elements:
            if start not in visited:
                cycle = []
                x = start

                while x not in visited:
                    visited.add(x)
                    cycle.append(x)
                    x = self.permutation.get(x, x)

                if len(cycle) > 1: 
                    cycles.append(Cycle(cycle))

        return CycleDecomposition(cycles, validated=True)

class Cycle:
    def __init__(self, elements, validated=False):
        """
        :param elements: List of elements in the cycle.
        :param validated: Boolean flag to indicate whether the cycle has already been validated.
        """
        self.elements = elements
        self.validated = validated
        if not validated:
            self.validate_cycle()

    def validate_cycle(self):
        """ Validate the cycle to ensure no repeated elements. """
        if len(set(self.elements)) != len(self.elements):
            raise InvalidCycleError(f"Cycle contains repeated elements: {self.elements}")
        elif not all(isinstance(e, int) for e in self.elements):
            raise NonIntegerElementError(f"Cycle contains non-integer elements: {self.elements}")
        self.validated = True

    def __len__(self):
        return len(self.elements)

    def __repr__(self):
        return f"Cycle({self.elements})"
    
    def display(self):
        return str(tuple(self.elements))

    def __eq__(self, other):
        if isinstance(other, Cycle):
            return self.elements == other.elements
        return False
    
    def invert(self):
        """Invert the cycle (reverse the order)."""
        return Cycle(self.elements[::-1])
    
    def to_transpositions(self):
        """
        Convert the cycle into a product of transpositions, where consecutive
        elements form transpositions.
        """
        transpositions = []
        # Create transpositions between consecutive elements in the cycle
        for i in range(len(self.elements) - 1):
            transpositions.append((self.elements[i], self.elements[i + 1]))
        return 
    
    def compose(self, other):
        """
        """
        if not isinstance(other, Cycle):
            raise TypeError("Can only compose with another Cycle.")
        else:
            return (self.to_permutation() * other.to_permutation()).to_cycles()
    
    def __mul__(self, other):
        return self.compose(other)
    
    def power(self, n):
        """
        Raise the cycle to the power of n by applying it n times.
        If n is negative, use the inverse of the cycle.
        :param n: The exponent to which to raise the cycle.
        :return: A new Cycle object representing the result of the power.
        """
        if n == 0:
            return Cycle([])

        if n < 0:
            return self.invert().power(-n)

        perm = self.to_permutation()
        perm_power = perm.power(n)
        return perm_power.to_cycles().cycles[0]
    
    def sign(self):
        """
        Calculate the sign of the cycle.
        :return: +1 if the cycle has an odd number of elements, -1 if even.
        """
        return (-1) ** (len(self) - 1)
    
    def order(self):
        """
        Calculate the order of the cycle.
        The order of a cycle is simply its length.
        :return: The order of the cycle.
        """
        return len(self)
    
    def to_permutation(self):
        mapping = {}
        for i in range(len(self.elements)):
            mapping[self.elements[i]] = self.elements[(i + 1) % len(self.elements)]
        return Permutation(mapping, validated=True)

class CycleDecomposition:
    def __init__(self, cycles, validated=False):
        """
        :param cycles: List of Cycle objects.
        :param validated: Boolean flag to indicate whether the decomposition has been validated.
        """
        self.cycles = cycles
        self.validated = validated
        if not validated:
            self.validate_decomposition()

    def validate_decomposition(self):
        """
        Validate the decomposition to ensure all cycles are disjoint.
        """
        element_set = set()
        for cycle in self.cycles:
            cycle_elements = set(cycle.elements)
            if element_set.intersection(cycle_elements):
                raise InvalidCycleDecompositionError(
                    "Cycle decomposition contains overlapping elements: "
                    f"{cycle_elements.intersection(element_set)}"
                )
            element_set.update(cycle_elements)
        self.validated = True

    def __len__(self):
        return len(self.cycles)

    def __repr__(self):
        return f"CycleDecomposition({self.cycles})"
    
    def __mul__(self, other):
        return self.compose(other)

    def power(self, n):
        """
        Raise the cycle decomposition to the power of n by applying each cycle n times.
        If n is negative, invert each cycle and apply |n|.
        :param n: The exponent to which to raise the decomposition.
        :return: A new CycleDecomposition representing the result of the power.
        """
        powered_cycles = [cycle.power(n) for cycle in self.cycles]
        return CycleDecomposition(powered_cycles, validated=True)
    
    def __eq__(self, other):
        if isinstance(other, CycleDecomposition):
            sorted_self_cycles = sorted([sorted(cycle.elements) for cycle in self.cycles])
            sorted_other_cycles = sorted([sorted(cycle.elements) for cycle in other.cycles])
            return sorted_self_cycles == sorted_other_cycles
        elif isinstance(other, Cycle) and len(self.cycles) == 1:
            return self.cycles[0] == other
        return False
    
    def to_permutation(self):
        """
        Convert cycle decomposition to a permutation.
        """
        mapping = {}
        for cycle in self.cycles:
            for i in range(len(cycle.elements)):
                mapping[cycle.elements[i]] = cycle.elements[(i + 1) % len(cycle.elements)]
        return Permutation(mapping, validated=True)
    
    def compose(self, other):
        """
        """
        return (self.to_permutation() * other.to_permutation()).to_cycles()
        
    def sign(self):
        """
        Calculate the sign of the permutation represented by this cycle decomposition.
        :return: +1 if the permutation is even, -1 if odd.
        """
        total_sign = 1
        for cycle in self.cycles:
            total_sign *= cycle.sign()
        return total_sign
    
    def order(self):
        """
        Calculate the order of the cycle decomposition.
        The order is the least common multiple (LCM) of the orders of its constituent cycles.
        :return: The order of the cycle decomposition.
        """
        if not self.cycles:
            return 1
        
        cycle_orders = [cycle.order() for cycle in self.cycles]

        def lcm(a, b):
            return abs(a * b) // gcd(a, b)

        return reduce(lcm, cycle_orders)
    
    def to_transpositions(self):
        """
        Convert the entire cycle decomposition into a list of transpositions.
        Collects transpositions from each cycle and concatenates them.
        """
        transpositions = []
        for cycle in self.cycles:
            transpositions.extend(cycle.to_transpositions())  # Get transpositions for each cycle
        return transpositions
