"""Compare entity-sample.tsv per organism between a released STF root (e.g. a yew final/) and a new one.

usage: python3 stf/compare_to_production.py <prodRoot> <newRoot> <detailOut.tsv>
"""
import csv
import os
import sys
from collections import Counter

prod_root, new_root, detail_out = sys.argv[1:4]
csv.field_size_limit(sys.maxsize)


def load(path):
    with open(path, newline='') as f:
        rows = list(csv.reader(f, delimiter='\t'))
    header = rows[0]
    return header, {r[0]: dict(zip(header, r)) for r in rows[1:]}


def orgs(root):
    return {f'{p}/{o}' for p in os.listdir(root) for o in os.listdir(os.path.join(root, p))}


shared = sorted(orgs(prod_root) & orgs(new_root))
print(f'only in new: {sorted(orgs(new_root) - orgs(prod_root))}')
print(f'only in prod: {sorted(orgs(prod_root) - orgs(new_root))}')
print('org\tprod_n\tnew_n\tids_removed\tids_added\tcols_removed\tcols_added\tcells_changed\ttop_changed_cols')
with open(detail_out, 'w') as d:
    d.write('org\tsample_id\tcolumn\tprod\tnew\n')
    for org in shared:
        ph, p = load(f'{prod_root}/{org}/entity-sample.tsv')
        nh, n = load(f'{new_root}/{org}/entity-sample.tsv')
        cols = [c for c in ph[1:] if c in nh]
        changed = Counter()
        for sid in p.keys() & n.keys():
            for c in cols:
                if p[sid][c] != n[sid][c]:
                    changed[c] += 1
                    d.write(f'{org}\t{sid}\t{c}\t{p[sid][c]}\t{n[sid][c]}\n')
        top = ','.join(f'{c}:{k}' for c, k in changed.most_common(4))
        print(f'{org}\t{len(p)}\t{len(n)}\t{len(p.keys() - n.keys())}\t{len(n.keys() - p.keys())}\t'
              f'{"|".join(c for c in ph if c not in nh)}\t{"|".join(c for c in nh if c not in ph)}\t'
              f'{sum(changed.values())}\t{top}')
