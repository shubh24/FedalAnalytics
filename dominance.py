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

def get_pressure_points(server, receiver):

	pressure_points = 0
	pressure_point_won = 0
	
	reference_score = ["0", "15", "30", "40", "AD"]

	for i, j in enumerate(server):

		current_server_score = j
		current_receiver_score = receiver[i]

		if (reference_score.index(current_receiver_score) - reference_score.index(current_server_score) >= 2) or (reference_score.index(current_receiver_score) - reference_score.index(current_server_score) == 1 and current_receiver_score in ("40", "AD")):
			
			pressure_points += 1

			if i+1 < len(server):				
				next_server_score = server[i+1]
				next_receiver_score = receiver[i+1]

				if next_server_score > current_server_score or (next_server_score == current_server_score and next_receiver_score < current_receiver_score):
					pressure_point_won += 1

	return (pressure_point_won, pressure_points)

# print get_pressure_points(["0", "0", "0", "0", "15", "30", "40", "40", "40", "40"], ["0", "15", "30", "40", "40", "40", "40", "AD", "40", "AD"])

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

			set_score = str(i.split(",")[1])
			total_sets = 1 + int(set_score.split("\xe2\x80\x91")[0]) + int(set_score.split("\xe2\x80\x91")[1])

		else:
			#Add dominance to global score
			game_count += 1
			server_dominance, receiver_dominance = calculate_dominance(server, receiver)
			pressure_points_won, pressure_points = get_pressure_points(server, receiver)

			if server_player.strip() == "Roger Federer":
				dominance.append([server_player.strip(), total_sets, game_count, server_dominance, receiver_dominance, pressure_points, pressure_points_won])

			elif server_player.strip() == "Rafael Nadal":
				dominance.append([server_player.strip(), total_sets, game_count, receiver_dominance, server_dominance, pressure_points, pressure_points_won])

			server = []
			receiver = []
			continue

	with open("dominance_pressure.csv", "w") as f:
		writer = csv.writer(f)
		writer.writerow(["server", "set_count", "game_count", "federer_dominance", "nadal_dominance", "pressure_points", "pressure_points_won"])
		writer.writerows(dominance)
