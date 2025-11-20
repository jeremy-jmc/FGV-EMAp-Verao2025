import sys
import os

def compare_files(file1, file2):
    with open(file1, 'r') as f1, open(file2, 'r') as f2:
        lines1 = f1.readlines()
        lines2 = f2.readlines()

    if len(lines1) != len(lines2):
        print(f"Files have different number of lines: {len(lines1)} vs {len(lines2)}")
        return False

    for i, (l1, l2) in enumerate(zip(lines1, lines2)):
        if l1.strip() != l2.strip():
            # Check for floating point differences
            try:
                val1 = float(l1.split(':')[1].strip().replace('$', '').replace('km', '').replace(',', ''))
                val2 = float(l2.split(':')[1].strip().replace('$', '').replace('km', '').replace(',', ''))
                if abs(val1 - val2) > 1e-1: # Allow small tolerance
                     print(f"Difference at line {i+1}:")
                     print(f"  File 1: {l1.strip()}")
                     print(f"  File 2: {l2.strip()}")
                     return False
            except (ValueError, IndexError):
                print(f"Difference at line {i+1}:")
                print(f"  File 1: {l1.strip()}")
                print(f"  File 2: {l2.strip()}")
                return False
    
    print("Files are identical (within tolerance).")
    return True

if __name__ == "__main__":
    base_path = "/home/tenken/Desktop/FGV-EMAp-Verao2025/optimizacao_julia/io1/results"
    instance = "25_c_1"
    phase = "forward_sweep"
    iteration = 0

    configurations = [
        ("ccw", "nsd"),
        ("ccw", "sd"),
        ("cw", "nsd"),
        ("cw", "sd"),
    ]

    all_passed = True
    for ccw_str, sd_str in configurations:
        config_str = f"{ccw_str}_{sd_str}"
        file_py = os.path.join(base_path, f"{instance}_{config_str}_phase_{phase}_it{iteration}_python.txt")
        file_cpp = os.path.join(base_path, f"{instance}_{config_str}_phase_{phase}_it{iteration}_cpp.txt")
        
        print(f"--- Comparing {config_str} ---")

        if not os.path.exists(file_py):
            print(f"File not found: {file_py}")
            all_passed = False
            continue
        
        if not os.path.exists(file_cpp):
            print(f"File not found: {file_cpp}")
            all_passed = False
            continue

        if not compare_files(file_py, file_cpp):
            all_passed = False
    
    print("\n" + "="*30)
    if all_passed:
        print("All forward_sweep configurations passed!")
        print("="*30)
    else:
        print("Some forward_sweep configurations failed.")
        print("="*30)
        sys.exit(1) # Exit if forward sweep fails
    