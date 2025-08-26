import torch
from tqdm import tqdm

def print_mat(matrix: torch.Tensor):
    """
    주어진 matrix에서 -1인 값을 "x"로 대체하여 출력합니다.
    모든 다른 값은 그대로 출력됩니다.
    """
    # 각 행을 순회하면서 -1은 "x"로, 그 외는 그대로 출력
    n,m = matrix.shape
    row_str = " ".join(f"{val:3d}" for val in range(0,m))
    print("\n<Matrix>")
    print(row_str)
    print("="*len(row_str))
    
    for i, row in enumerate(matrix.tolist()):
        row_str = " ".join(f"{'x':>3}" if val == -1 else f"{val:3d}" for val in row)
        row_str = row_str + " ||" + str(i)
        print(row_str)

def density_check(matrix):
    n,m = matrix.shape
    total = n * m
    zero = 0
    for row in matrix.tolist():
        zero += row.count(-1)

    nonzero = total - zero
    return nonzero/total

def remove_empty(matrix: torch.Tensor) -> torch.Tensor:
    col_mask = ~(matrix == -1).all(dim=0)
    row_mask = ~(matrix == -1).all(dim=1)
    return matrix[row_mask][:, col_mask]

def sparse_matrix(n,m,d):
    """
    zero가 -1이고 그외가 nonzero인 Sparse 행렬 생성
    n: 행렬의 행 개수
    m: 행렬의 열 개수
    d: 행렬의 density (0~1의 소수)
    """
    matrix = torch.randn(n, m)
    mask = (torch.rand(n, m) < d).to(matrix.dtype)
    pruned_matrix = matrix * mask

    col_indices = torch.arange(m, dtype=torch.int32).unsqueeze(0).expand(n, m)
    mask_int = mask.to(torch.int32)  # True -> 1, False -> 0
    result = torch.where(mask_int.bool(), col_indices, torch.full_like(col_indices, -1))

    return result

def align_matrix(matrix):
    n_rows, n_cols = matrix.shape
    aligned_matrix = torch.full((n_rows, n_cols), -1, dtype=matrix.dtype, device=matrix.device)

    for i, row in enumerate(matrix):
        nonzeros = row[row > -1]
        aligned_matrix[i, :len(nonzeros)] = nonzeros

    return aligned_matrix

if __name__ == '__main__':
    
    #Sparse 행렬 생성
    """
    n: 행렬의 행 개수
    m: 행렬의 열 개수
    d: 행렬의 Density
    """
    n = 4 
    m = 16
    d_list = [0.2]
    
    #실험 환경
    """
    r: 실험 반복 횟수
    """
    r = 5
    p = True

    for d in d_list:
        avg = 0
        for i in range(0,r):
            matrix = sparse_matrix(n, m, d)
            matrix = align_matrix(matrix)
            matrix = remove_empty(matrix)
            density = density_check(matrix)
            avg += density
            if p:
                print_mat(matrix)
                print(f"Density Improve from {d:.2f} to {density:.2f}!!")
        avg = avg / r
        print(f"AVG: {d:.2f} to {avg:.2f}!!")
