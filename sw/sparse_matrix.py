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
    
def column_combine(matrix, max_conflict, mux_size, P=False):
    matrix = remove_empty(matrix)
    n_rows, n_cols = matrix.shape
    # 각 열의 nonzero row index 집합 (matrix >= 0)
    nonzero = [set(torch.where(matrix[:, ci] >= 0)[0].tolist()) for ci in range(n_cols)]

    groups = []       # 각 그룹의 union된 row index 집합
    gidx = []         # 각 그룹에 속한 열 인덱스 리스트
    gconf = []        # 각 그룹의 누적 conflict 수

    for ci in range(n_cols):
        if not nonzero[ci]:
            continue
        col_set = nonzero[ci]
        best_improve, best_grp, best_union, best_conflict = 0, -1, None, 0
        for gi, grp in enumerate(groups):
            union_set = grp | col_set
            improve = len(union_set) - len(grp)
            conflict = len(grp & col_set)
            if (conflict + gconf[gi] <= max_conflict) and (len(gidx[gi]) < mux_size):
                if improve > best_improve:
                    best_improve, best_grp, best_union, best_conflict = improve, gi, union_set, conflict
        if best_grp < 0:
            groups.append(set(col_set))
            gidx.append([ci])
            gconf.append(0)
        else:
            gidx[best_grp].append(ci)
            groups[best_grp] = best_union
            gconf[best_grp] += best_conflict

    packed = torch.full((len(groups), n_rows), -1, dtype=torch.int32)
    for gi, cols in enumerate(gidx):
        for ci in cols:
            for ri in nonzero[ci]:
                packed[gi, ri] = ci

    group_len = [len(lst) for lst in gidx if lst]

    packed = packed.transpose(0,1)

    if P:
        print("\n <<Column Combine Sparse Matrix>>")
        print_mat(packed)

    density_check(packed)

    return packed, group_len, gidx

def eureka(matrix):
    n_rows, n_cols = matrix.shape
    if n_rows > 0 and n_cols > 0:
        aligned_matrix = torch.full((n_rows, n_cols), -1, dtype=matrix.dtype, device=matrix.device)

        for i, row in enumerate(matrix):
            nonzeros = row[row > -1]
            aligned_matrix[i, :len(nonzeros)] = nonzeros

        # 각 행의 -1보다 큰 값의 개수를 센다.
        nonzero_counts = (aligned_matrix > -1).sum(dim=1)
        avg_nonzero = math.ceil(nonzero_counts.sum().item() / n_rows)

        rebalanced_matrix = aligned_matrix.clone()

        # 아래 행부터 위로 올라가며 부족한 값을 위 행에서 가져온다.
        for row in range(n_rows - 1, 0, -1):
            count_row = nonzero_counts[row].item()
            if count_row < avg_nonzero:
                needed = avg_nonzero - count_row
                upper_row = row - 1

                available = nonzero_counts[upper_row].item()
                take = min(available, needed)

                if take > 0:
                    # 상위 행의 사용 가능한 값들 (앞쪽부터 available개)
                    nonzero_values = rebalanced_matrix[upper_row, :available]
                    # 현재 행에 상위 행의 값 중 뒤쪽 take개를 할당 (혹은 nonzero_values[:take]로 앞쪽 값을 가져올 수도 있음)
                    start = count_row
                    end = count_row + take
                    rebalanced_matrix[row, start:end] = nonzero_values[-take:]

                    # 상위 행에서 가져간 부분은 제거하고 -1로 채움
                    if take < available:
                        remaining = nonzero_values[:-take]
                    else:
                        remaining = torch.tensor([], dtype=matrix.dtype, device=matrix.device)
                    filler = torch.full((take,), -1, dtype=matrix.dtype, device=matrix.device)
                    new_upper = torch.cat((remaining, filler))
                    rebalanced_matrix[upper_row, :available] = new_upper

                    # nonzero_counts 업데이트 (텐서 요소이므로 + 연산 가능)
                    nonzero_counts[row] = nonzero_counts[row] + take
                    nonzero_counts[upper_row] = nonzero_counts[upper_row] - take

        # print_mat는 외부에서 정의된 함수라고 가정합니다.
        #print_mat(aligned_matrix)

        return remove_col(rebalanced_matrix)
        
def inter_tile_fill(now_t, next_t, mux_size, sa_size = 16):
    now_t_l = reorder_tensor(now_t,"d")
    now_t_l, _, now_col_in_grps = column_combine(now_t_l, 0, mux_size, False)

    next_t_l = reorder_tensor(next_t, "a")
    next_col_descend = torch.argsort((next_t_l > -1).sum(dim=0), descending=True).tolist()

    _,now_nz,_=density_check(now_t)
    _,next_nz,_=density_check(next_t)
    nonzeros = now_nz + next_nz

    for i, col in enumerate(now_t_l.transpose(0,1)):
        if i >= sa_size/2-1:
            next_col_able = now_col_in_grps[i]
            next_col_able = sorted(next_col_able, key=next_col_descend.index)
            for j, val in enumerate(col):
                if val == -1:
                    able_cols = [(next_col_able[i],x) for i, x in enumerate(next_t_l[j, next_col_able]) if x > -1]
                    if able_cols:
                        index, nonzero = able_cols[0]
                        now_t_l[j,i] = nonzero
                        next_t_l[j,index] = -1
                        #print("Move: ",(j,i)," <- ",(j,int(able_cols[index])),int(nonzero))

    _,n_now_nz,_=density_check(now_t_l)
    _,n_next_nz,_=density_check(next_t_l)

    if nonzeros != (n_now_nz + n_next_nz):
        print(">>>>Row Scatter Error. Please Edit Code",)
        print(">> "+str(now_nz)+"->"+str(n_now_nz)+" = "+str(n_now_nz-now_nz))
        print(">> "+str(next_nz)+"->"+str(n_next_nz)+" = "+str(next_nz-n_next_nz))

    return now_t_l, next_t_l

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
