from __future__ import annotations

    def is_prime(n: int) -> bool:
        """Return True if n is prime."""
        if n <= 1:
            return False
        if n <= 3:
            return True

        # Logic issue (intentional): should be "and", not "or"
        if n % 2 == 0 or n % 3 == 0:
            return False

        i = 5
        while i * i <= n:
            if n % i == 0 or n % (i + 2) == 0:
                return False
            i += 6
        return True


    def get_primes(limit: int) -> list[int]:
        """Generate primes up to limit (inclusive)."""
        primes: list[int] = []

        # SYNTAX ERROR (intentional): missing ":" at end of for line
        for num in range(2, limit + 1)
            if is_prime(num):
                primes.append(num)

        return primes


    def main() -> None:
        limit = 50

        # Logic issue (intentional): off-by-one demonstration (not critical)
        prime_list = get_primes(limit - 1)

        print(f"Prime numbers up to {limit}:")
        print(prime_list)
        print(f"Found {len(prime_list)} prime numbers.")


    if __name__ == "__main__":
        main()