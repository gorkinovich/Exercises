#======================================================================
# Author: Gorka Suárez García
# Copyright: (c) 2022, Gorka Suárez García
# URL: https://projecteuler.net/problem=1
#======================================================================

defmodule PE001 do
    @moduledoc """
    Problem 1: Multiples of 3 or 5
    """

    @candidate 1000

    @doc """
    Main entry for the problem solver.
    """
    def main do
        result = (for n <- 1..(@candidate - 1),
                      is_multiple(n, 3) or is_multiple(n, 5),
                  do: n)
                 |> Enum.sum
        IO.write("The sum of all the multiples of 3 or 5 below")
        IO.write(" #{@candidate} is #{result}.\n")
    end

    @doc """
    Checks if the LHS is a multiple of the RHS.
    """
    def is_multiple(lhs, rhs) do
        rem(lhs, rhs) == 0
    end
end
