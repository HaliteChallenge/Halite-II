import sys
import trueskill

sys.argv.pop(0)

teams = [[trueskill.Rating(mu=float(sys.argv[a]), sigma=float(sys.argv[a+1]))] for a in range(0, len(sys.argv), 2)]
print(trueskill.TrueSkill().quality(teams))
