import csv
import string

exclist = string.punctuation + string.digits
# remove punctuations and digits
table_ = str.maketrans('', '', exclist)

name_pos_dict = {}
pos_set = set()
with open('player_position.csv', 'r', encoding='utf-8', newline='') as all_final_f:
    reader = csv.reader(all_final_f, delimiter=',')
    for row in reader:
        name = str(row[1]).lower().translate(table_)
        pos = str(row[2])
        pos_set.add(pos)
        name_pos_dict[name] = pos

full_player_set = set()
player_set = set()
id_set = set()
with open('2019-2020_players.csv', 'r', newline='') as players_f:
    with open('2019-2020_ANNO_Players.csv', 'w', newline='') as anno_player_f:
        reader = csv.reader(players_f, delimiter=',')
        reader = list(reader)
        fieldnames = reader[0]
        fieldnames.append('POSITION')
        print(fieldnames)
        writer = csv.DictWriter(anno_player_f, fieldnames=fieldnames)
        writer.writeheader()
        for row in reader[1:]:
            print(row)
            name = str(row[0]).lower().translate(table_)
            p_ID = str(row[2])
            id_set.add(p_ID)
            full_player_set.add(name)
            if name not in name_pos_dict:
                player_set.add(name)
                writer.writerow({'PLAYER_NAME': row[0], 'TEAM_ID': row[1], 'PLAYER_ID': row[2], 'SEASON': row[3],
                                 'POSITION': ''})
            else:
                writer.writerow({'PLAYER_NAME': row[0], 'TEAM_ID': row[1], 'PLAYER_ID': row[2], 'SEASON': row[3],
                                 'POSITION': name_pos_dict[name]})
            #

    print(pos_set)
    print(len(full_player_set))
    print(len(player_set))
    print(player_set)

full_name_set = set()
name_set = set()
left_id_set = set()
with open('games_details.csv', 'r', newline='') as game_details_f:
    reader = csv.reader(game_details_f, delimiter=',')
    for row in reader:
        name = str(row[5]).lower().translate(table_)
        p_ID = str(row[4])

        if p_ID not in id_set:
            left_id_set.add(p_ID)

        full_name_set.add(name)
        if name not in full_player_set:
            name_set.add(name)

    print(len(full_name_set))
    print(len(name_set))
    print(len(left_id_set))
    print(left_id_set)
