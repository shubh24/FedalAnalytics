from __future__ import division
import csv

def calculate_dominance(server, receiver):

	prev_server_score = "0"
	prev_receiver_score = "0"

	server_points = 0
	receiver_points = 0

	for i, j in enumerate(server):

		current_server_score = j
		current_receiver_score = receiver[i]

		if current_server_score > prev_server_score and current_receiver_score == prev_receiver_score:
			server_points += 1
		elif current_server_score == prev_server_score and current_receiver_score > prev_receiver_score:
			receiver_points += 1
		elif current_server_score < prev_server_score and current_receiver_score == prev_receiver_score:
			receiver_points += 1
		elif current_server_score == prev_server_score and current_receiver_score < prev_receiver_score:
			server_points += 1

		prev_server_score = current_server_score
		prev_receiver_score = current_receiver_score

	if current_server_score > current_receiver_score:
		server_points += 1
	else:
		receiver_points += 1

	return (round(server_points/(server_points+receiver_points), 2), round(receiver_points/(server_points+receiver_points), 2))

# print calculate_dominance(["0", "15", "30", "40", "40", "40", "40", "Ad", "40", "Ad"], ["0", "0", "0", "0", "15", "30", "40", "40", "40", "40"])

with open("./ChartingData/AusOpen/point_by_point.csv", "r") as f:

	server = []
	receiver = []

	dominance = []
	game_count = 0

	for i in f.readlines():

		if str(i.split(",")[0]) != " ":
			server_player = str(i.split(",")[0])
			score = str(i.split(",")[3])
			server.append(score.split("\xe2\x80\x91")[0])
			receiver.append(score.split("\xe2\x80\x91")[1])
		else:
			#Add dominance to global score
			game_count += 1
			server_dominance, receiver_dominance = calculate_dominance(server, receiver)

			if server_player.strip() == "Roger Federer":
				dominance.append([server_player.strip(), game_count, server_dominance, receiver_dominance])

			elif server_player.strip() == "Rafael Nadal":
				dominance.append([server_player.strip(), game_count, receiver_dominance, server_dominance])

			server = []
			receiver = []
			continue

	with open("dominance.csv", "w") as f:
		writer = csv.writer(f)
		writer.writerow(["server", "game_count", "federer_dominance", "nadal_dominance"])
		writer.writerows(dominance)
