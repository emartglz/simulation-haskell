from matplotlib import pyplot as plt
import numpy as np


def main():
    t = [i for i in range(1, 26)]
    a1 = [
        144,
        140,
        119,
        123,
        123,
        131,
        110,
        115,
        114,
        106,
        107,
        111,
        111,
        106,
        119,
        109,
        130,
        109,
        109,
        109,
        108,
        107,
        118,
        115,
        115,
    ]
    a2 = [
        186,
        145,
        144,
        133,
        128,
        134,
        121,
        123,
        115,
        117,
        118,
        112,
        110,
        109,
        108,
        106,
        110,
        106,
        105,
        108,
        103,
        106,
        108,
        109,
        103,
    ]
    a3 = [
        176,
        137,
        124,
        130,
        128,
        128,
        127,
        134,
        114,
        121,
        126,
        121,
        126,
        118,
        113,
        131,
        110,
        122,
        118,
        118,
        117,
        111,
        121,
        118,
        111,
    ]

    plt.plot(t, a1, label="prefer child")
    plt.plot(t, a2, label="prefer trash")
    plt.plot(t, a3, label="prefer closer")
    plt.xlabel("T")
    plt.ylabel("turnos")
    plt.legend()
    plt.savefig("simulation.pdf")


if __name__ == "__main__":
    main()
