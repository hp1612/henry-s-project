import os
import random
import string

OUT_DIR = r"C:\Users\85251\Documents\Pipe_File" #output path
os.makedirs(OUT_DIR, exist_ok=True)


def generate_pip(version, asset_prefix) -> str:
    """Generate one .data file and return its path."""
    filename = f"{version - 100}.data" #set the file name
    path = os.path.join(OUT_DIR, filename) #combine file path and file name to be a full path
    n_assets = 100
    print(f"Writing: {path}")

    with open(path, "w", encoding="utf-8") as f: #encoding is safer when we have non-ascii characters
        # header line
        f.write(f"[version]={version}\n")
        f.write("asset|exp_yield|residual\n")

        # body lines
        for i in range(1, n_assets + 1):
            asset = f"{asset_prefix}{i:03d}" #001, 099, 100
            exp_yield = f"{random.uniform(0.1, 10.0):.6f}"
            residual = f"{random.uniform(-1.5, 1.0):.6f}"
            f.write(f"{asset}|{exp_yield}|{residual}\n")

        # end marker
        f.write("-END-\n")

    return path


def main():
    # j = 1..10  → versions 101..110, types A..B
    for j in range(1, 11):
        version = j + 100
        asset_prefix = string.ascii_uppercase[j - 1]  # get A, B, ...
        generate_pip(version, asset_prefix)


if __name__ == "__main__":
    main()



