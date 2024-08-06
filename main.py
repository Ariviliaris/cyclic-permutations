class Permutation:
    def __init__(self, mapping):
        """
        Initialize the permutation with a mapping.

        :param mapping: A dictionary where keys and values are integers representing the permutation.
        """
        self.permutation = {i + 1: mapping[i + 1] for i in range(len(mapping))}

    def __repr__(self):
        return f"Permutation({self.permutation})"

    def inverse(self):
        """ Return the inverse of the permutation. """
        inverse_mapping = {v: k for k, v in self.permutation.items()}
        return Permutation(inverse_mapping)

    def compose(self, other):
        """ Compose this permutation with another permutation. """
        composed_mapping = {k: self.permutation[other.permutation[k]] for k in self.permutation}
        return Permutation(composed_mapping)


# Driver code
# Note: try identity permutations

sigma = Permutation({1: 2, 2: 3, 3: 1})
tau = Permutation({1: 2, 2: 1, 3: 3})
sigma_inv = sigma.inverse()
sigma_tau = sigma.compose(tau)

print("Sigma:", sigma)
print("Tau:", tau)
print("Sigma Inverse:", sigma_inv)
print("Sigma composed with Tau:", sigma_tau)
