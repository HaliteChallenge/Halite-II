import sys

sys.argv.pop(0)

teams = []
for index in range(0, len(sys.argv), 2):
    teams.append([trueskill.Rating(mu=float(sys.argv[index]), sigma=float(sys.argv[index+1]))])

newRatings = trueskill.rate(teams)
for rating in newRatings:
    print(str(rating[0].mu) + " " + str(rating[0].sigma))
