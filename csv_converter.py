import csv

county = "Lee"
file = "/Users/derekwillis/code/openelections-sources-fl/2020/general/Lee FL CandidateResultsbyPrecinctandParty_2020-11-04T03_09_46_754c90c4-c423-41c4-9a01-07cccdb4d42d.csv"
results = []

RAW_OFFICE_TO_OFFICE_AND_DISTRICT = {
    'President and Vice President': ('President', ''),
    'President/Vice President': ('President', ''),
    'Representative in Congress District 1': ('U.S. House', 1),
    'Representative in Congress, District 1': ('U.S. House', 1),
    'Representative in Congress District 2': ('U.S. House', 2),
    'Representative in Congress District 3': ('U.S. House', 3),
    'Representative in Congress, District 3': ('U.S. House', 3),
    'Representative in Congress District 4': ('U.S. House', 4),
    'Representative in Congress District 5': ('U.S. House', 5),
    'Representative in Congress District 6': ('U.S. House', 6),
    'Representative in Congress District 7': ('U.S. House', 7),
    'Representative in Congress District 8': ('U.S. House', 8),
    'Representative in Congress District 9': ('U.S. House', 9),
    'Representative in Congress, District 9': ('U.S. House', 9),
    'Representative in Congress District 10': ('U.S. House', 10),
    'Representative in Congress District 11': ('U.S. House', 11),
    'Representative in Congress District 12': ('U.S. House', 12),
    'Representative in Congress District 13': ('U.S. House', 13),
    'Representative in Congress District 14': ('U.S. House', 14),
    'Representative in Congress District 15': ('U.S. House', 15),
    'Representative in Congress, District 15': ('U.S. House', 15),
    'Representative in Congress District 16': ('U.S. House', 16),
    'Representative in Congress, District 16': ('U.S. House', 16),
    'Representative in Congress District 17': ('U.S. House', 17),
    'Representative in Congress, District 17': ('U.S. House', 17),
    'Representative in Congress District 18': ('U.S. House', 18),
    "United States Representative in Congress, District 18": ('U.S. House', 18),
    'Representative in Congress District 19': ('U.S. House', 19),
    'Representative in Congress District 20': ('U.S. House', 20),
    'Representative in Congress District 21': ('U.S. House', 21),
    'Representative in Congress District 22': ('U.S. House', 22),
    'Representative in Congress District 23': ('U.S. House', 23),
    'Representative in Congress District 24': ('U.S. House', 24),
    'Representative Congress Dist 24': ('U.S. House', 24),
    'Representative Congress Dist 26': ('U.S. House', 26),
    'Representative Congress Dist 27': ('U.S. House', 27),
    'Representative in Congress District 25': ('U.S. House', 25),
    'Representative in Congress District 26': ('U.S. House', 26),
    'Representative in Congress District 27': ('U.S. House', 27),
    'Representative in Congress, Dist 1': ('U.S. House', 1),
    'Representative in Congress, Dist 2': ('U.S. House', 2),
    'Representative in Congress, Dist 3': ('U.S. House', 3),
    'Representative in Congress, Dist 4': ('U.S. House', 4),
    'Representative in Congress, Dist 5': ('U.S. House', 5),
    'Representative in Congress, Dist 6': ('U.S. House', 6),
    'Representative in Congress, Dist 7': ('U.S. House', 7),
    'Representative in Congress, Dist 8': ('U.S. House', 8),
    'Representative in Congress, Dist 9': ('U.S. House', 9),
    'Representative in Congress, Dist 10': ('U.S. House', 10),
    'Representative in Congress, Dist 11': ('U.S. House', 11),
    'Representative in Congress, Dist 12': ('U.S. House', 12),
    'Representative in Congress, Dist 13': ('U.S. House', 13),
    'Representative in Congress, Dist 14': ('U.S. House', 14),
    'Representative in Congress, Dist 15': ('U.S. House', 15),
    'Representative in Congress, Dist 16': ('U.S. House', 16),
    'Representative in Congress, Dist 17': ('U.S. House', 17),
    'Representative in Congress, Dist 18': ('U.S. House', 18),
    'Representative in Congress, Dist 19': ('U.S. House', 19),
    'Representative in Congress, Dist 20': ('U.S. House', 20),
    'Representative in Congress, Dist 21': ('U.S. House', 21),
    'Representative in Congress, Dist 22': ('U.S. House', 22),
    'Representative in Congress, Dist 23': ('U.S. House', 23),
    'Representative in Congress, Dist 24': ('U.S. House', 24),
    'Representative in Congress, Dist 25': ('U.S. House', 25),
    'Representative in Congress, Dist 26': ('U.S. House', 26),
    'Representative in Congress, Dist 27': ('U.S. House', 27),
    'State Senator District 1': ('State Senate', 1),
    'State Senator District 2': ('State Senate', 2),
    'State Senator District 3': ('State Senate', 3),
    'State Senator District 4': ('State Senate', 4),
    'State Senator District 5': ('State Senate', 5),
    'State Senator District 6': ('State Senate', 6),
    'State Senator District 7': ('State Senate', 7),
    'State Senator District 8': ('State Senate', 8),
    'State Senator District 9': ('State Senate', 9),
    'State Senator District 10': ('State Senate', 10),
    'State Senator District 11': ('State Senate', 11),
    'State Senator District 12': ('State Senate', 12),
    'State Senator District 13': ('State Senate', 13),
    'State Senator District 14': ('State Senate', 14),
    'State Senator District 15': ('State Senate', 15),
    'State Senator District 16': ('State Senate', 16),
    'State Senator District 17': ('State Senate', 17),
    'State Senator District 18': ('State Senate', 18),
    'State Senator District 19': ('State Senate', 19),
    'State Senator District 20': ('State Senate', 20),
    'State Senator, District 20': ('State Senate', 20),
    'State Senator District 21': ('State Senate', 21),
    'State Senator, District 21': ('State Senate', 21),
    'State Senator District 22': ('State Senate', 22),
    'State Senator District 23': ('State Senate', 23),
    'State Senator District 24': ('State Senate', 24),
    'State Senator District 25': ('State Senate', 25),
    'State Senator for Senator District 25': ('State Senate', 25),
    'State Senator District 26': ('State Senate', 26),
    'State Senator District 27': ('State Senate', 27),
    'State Senator District 28': ('State Senate', 28),
    'State Senator District 29': ('State Senate', 29),
    'State Senator District 30': ('State Senate', 30),
    'State Senator District 31': ('State Senate', 31),
    'State Senator District 32': ('State Senate', 32),
    'State Senator District 33': ('State Senate', 33),
    'State Senator District 34': ('State Senate', 34),
    'State Senator District 35': ('State Senate', 35),
    'State Senator District 36': ('State Senate', 36),
    'State Senator District 37': ('State Senate', 37),
    'State Senator District 38': ('State Senate', 38),
    'State Senator District 39': ('State Senate', 39),
    'State Senator District 40': ('State Senate', 40),
    'State Senator, Dist. 1': ('State Senate', 1),
    'State Senator, Dist. 2': ('State Senate', 2),
    'State Senator, Dist. 3': ('State Senate', 3),
    'State Senator, Dist. 4': ('State Senate', 4),
    'State Senator, Dist. 5': ('State Senate', 5),
    'State Senator, Dist. 6': ('State Senate', 6),
    'State Senator, Dist. 7': ('State Senate', 7),
    'State Senator, Dist. 8': ('State Senate', 8),
    'State Senator, Dist. 9': ('State Senate', 9),
    'State Senator, Dist. 10': ('State Senate', 10),
    'State Senator, Dist. 11': ('State Senate', 11),
    'State Senator, Dist. 12': ('State Senate', 12),
    'State Senator, Dist. 13': ('State Senate', 13),
    'State Senator, Dist. 14': ('State Senate', 14),
    'State Senator, Dist. 15': ('State Senate', 15),
    'State Senator, Dist. 16': ('State Senate', 16),
    'State Senator, Dist. 17': ('State Senate', 17),
    'State Senator, Dist. 18': ('State Senate', 18),
    'State Senator, Dist. 19': ('State Senate', 19),
    'State Senator, Dist. 20': ('State Senate', 20),
    'State Senator, Dist. 21': ('State Senate', 21),
    'State Senator, Dist. 22': ('State Senate', 22),
    'State Senator, Dist. 23': ('State Senate', 23),
    'State Senator, Dist. 24': ('State Senate', 24),
    'State Senator, Dist. 25': ('State Senate', 25),
    'State Senator, Dist. 26': ('State Senate', 26),
    'State Senator, Dist. 27': ('State Senate', 27),
    'State Senator, Dist. 28': ('State Senate', 28),
    'State Senator, Dist. 29': ('State Senate', 29),
    'State Senator, Dist. 30': ('State Senate', 30),
    'State Senator, Dist. 31': ('State Senate', 31),
    'State Senator, Dist. 32': ('State Senate', 32),
    'State Senator, Dist. 33': ('State Senate', 33),
    'State Senator, Dist. 34': ('State Senate', 34),
    'State Senator, Dist. 35': ('State Senate', 35),
    'State Senator Dist 35': ('State Senate', 35),
    'State Senator, Dist. 36': ('State Senate', 36),
    'State Senator, Dist. 37': ('State Senate', 37),
    'State Senator Dist 37': ('State Senate', 37),
    'State Senator, Dist. 38': ('State Senate', 38),
    'State Senator, Dist. 39': ('State Senate', 39),
    'State Senator Dist 39': ('State Senate', 39),
    'State Senator, Dist. 40': ('State Senate', 40),
    'State Representative District 1': ('State House', 1),
    'State Representative District 2': ('State House', 2),
    'State Representative District 3': ('State House', 3),
    'State Representative District 4': ('State House', 4),
    'State Representative District 5': ('State House', 5),
    'State Representative District 6': ('State House', 6),
    'State Representative District 7': ('State House', 7),
    'State Representative District 8': ('State House', 8),
    'State Representative District 9': ('State House', 9),
    'State Representative District 10': ('State House', 10),
    'State Representative District 11': ('State House', 11),
    'State Representative District 12': ('State House', 12),
    'State Representative District 13': ('State House', 13),
    'State Representative District 14': ('State House', 14),
    'State Representative District 15': ('State House', 15),
    'State Representative District 16': ('State House', 16),
    'State Representative District 17': ('State House', 17),
    'State Representative District 18': ('State House', 18),
    'State Representative District 19': ('State House', 19),
    'State Representative, District 19': ('State House', 19),
    'State Representative District 20': ('State House', 20),
    'State Representative District 21': ('State House', 21),
    'State Representative District 22': ('State House', 22),
    'State Representative District 23': ('State House', 23),
    'State Representative District 24': ('State House', 24),
    'State Representative District 25': ('State House', 25),
    'State Representative District 26': ('State House', 26),
    'State Representative District 27': ('State House', 27),
    'State Representative District 28': ('State House', 28),
    'State Representative District 29': ('State House', 29),
    'State Representative District 30': ('State House', 30),
    'State Representative District 31': ('State House', 31),
    'State Representative District 32': ('State House', 32),
    'State Representative District 33': ('State House', 33),
    'State Representative District 34': ('State House', 34),
    'State Representative District 35': ('State House', 35),
    'State Representative District 36': ('State House', 36),
    'State Representative District 37': ('State House', 37),
    'State Representative District 38': ('State House', 38),
    'State Representative District 39': ('State House', 39),
    'State Representative District 40': ('State House', 40),
    'State Representative District 41': ('State House', 41),
    'State Representative District 42': ('State House', 42),
    'State Representative District 43': ('State House', 43),
    'State Representative District 44': ('State House', 44),
    'State Representative District 45': ('State House', 45),
    'State Representative District 46': ('State House', 46),
    'State Representative District 47': ('State House', 47),
    'State Representative District 48': ('State House', 48),
    'State Representative District 49': ('State House', 49),
    'State Representative District 50': ('State House', 50),
    'State Representative District 51': ('State House', 51),
    'State Representative District 52': ('State House', 52),
    'State Representative District 53': ('State House', 53),
    'State Representative District 54': ('State House', 54),
    'State Representative for House District 54': ('State House', 54),
    'State Representative District 55': ('State House', 55),
    'State Representative for House District 55': ('State House', 55),
    'State Representative District 56': ('State House', 56),
    'State Representative District 57': ('State House', 57),
    'State Representative District 58': ('State House', 58),
    'State Representative District 59': ('State House', 59),
    'State Representative District 60': ('State House', 60),
    'State Representative District 61': ('State House', 61),
    'State Representative District 62': ('State House', 62),
    'State Representative District 63': ('State House', 63),
    'State Representative District 64': ('State House', 64),
    'State Representative District 65': ('State House', 65),
    'State Representative District 66': ('State House', 66),
    'State Representative District 67': ('State House', 67),
    'State Representative District 68': ('State House', 68),
    'State Representative District 69': ('State House', 69),
    'State Representative District 70': ('State House', 70),
    'State Representative District 71': ('State House', 71),
    'State Representative District 72': ('State House', 72),
    'State Representative District 73': ('State House', 73),
    'State Representative District 74': ('State House', 74),
    'State Representative District 75': ('State House', 75),
    'State Representative District 76': ('State House', 76),
    'State Representative District 77': ('State House', 77),
    'State Representative District 78': ('State House', 78),
    'State Representative District 79': ('State House', 79),
    'State Representative District 80': ('State House', 80),
    'State Representative District 81': ('State House', 81),
    'State Representative District 82': ('State House', 82),
    'State Representative District 83': ('State House', 83),
    'State Representative for House District 83': ('State House', 83),
    'State Representative District 84': ('State House', 84),
    'State Representative for House District 84': ('State House', 84),
    'State Representative District 85': ('State House', 85),
    'State Representative District 86': ('State House', 86),
    'State Representative District 87': ('State House', 87),
    'State Representative District 88': ('State House', 88),
    'State Representative District 89': ('State House', 89),
    'State Representative District 90': ('State House', 90),
    'State Representative District 91': ('State House', 91),
    'State Representative District 92': ('State House', 92),
    'State Representative District 93': ('State House', 93),
    'State Representative District 94': ('State House', 94),
    'State Representative District 95': ('State House', 95),
    'State Representative District 96': ('State House', 96),
    'State Representative District 97': ('State House', 97),
    'State Representative District 98': ('State House', 98),
    'State Representative District 99': ('State House', 99),
    'State Representative District 100': ('State House', 100),
    'State Representative District 101': ('State House', 101),
    'State Representative District 102': ('State House', 102),
    'State Representative District 103': ('State House', 103),
    'State Representative District 104': ('State House', 104),
    'State Representative District 105': ('State House', 105),
    'State Representative District 106': ('State House', 106),
    'State Representative District 107': ('State House', 107),
    'State Representative District 108': ('State House', 108),
    'State Representative District 109': ('State House', 109),
    'State Representative District 110': ('State House', 110),
    'State Representative District 111': ('State House', 111),
    'State Representative District 112': ('State House', 112),
    'State Representative District 113': ('State House', 113),
    'State Representative District 114': ('State House', 114),
    'State Representative District 115': ('State House', 115),
    'State Representative District 116': ('State House', 116),
    'State Representative District 117': ('State House', 117),
    'State Representative District 118': ('State House', 118),
    'State Representative District 119': ('State House', 119),
    'State Representative District 120': ('State House', 120),
    'State Representative, Dist 1': ('State House', 1),
    'State Representative, Dist 2': ('State House', 2),
    'State Representative, Dist 3': ('State House', 3),
    'State Representative, Dist 4': ('State House', 4),
    'State Representative, Dist 5': ('State House', 5),
    'State Representative, Dist 6': ('State House', 6),
    'State Representative, Dist 7': ('State House', 7),
    'State Representative, Dist 8': ('State House', 8),
    'State Representative, Dist 9': ('State House', 9),
    'State Representative, Dist 10': ('State House', 10),
    'State Representative, Dist 11': ('State House', 11),
    'State Representative, Dist 12': ('State House', 12),
    'State Representative, Dist 13': ('State House', 13),
    'State Representative, Dist 14': ('State House', 14),
    'State Representative, Dist 15': ('State House', 15),
    'State Representative, Dist 16': ('State House', 16),
    'State Representative, Dist 17': ('State House', 17),
    'State Representative, Dist 18': ('State House', 18),
    'State Representative, Dist 19': ('State House', 19),
    'State Representative, Dist 20': ('State House', 20),
    'State Representative, Dist 21': ('State House', 21),
    'State Representative, Dist 22': ('State House', 22),
    'State Representative, Dist 23': ('State House', 23),
    'State Representative, Dist 24': ('State House', 24),
    'State Representative, Dist 25': ('State House', 25),
    'State Representative, Dist 26': ('State House', 26),
    'State Representative, Dist 27': ('State House', 27),
    'State Representative, Dist 28': ('State House', 28),
    'State Representative, Dist 29': ('State House', 29),
    'State Representative, Dist 30': ('State House', 30),
    'State Representative, Dist 31': ('State House', 31),
    'State Representative, Dist 32': ('State House', 32),
    'State Representative, Dist 33': ('State House', 33),
    'State Representative, Dist 34': ('State House', 34),
    'State Representative, Dist 35': ('State House', 35),
    'State Representative, Dist 36': ('State House', 36),
    'State Representative, Dist 37': ('State House', 37),
    'State Representative, Dist 38': ('State House', 38),
    'State Representative, Dist 39': ('State House', 39),
    'State Representative, District 39': ('State House', 39),
    'State Representative, Dist 40': ('State House', 40),
    'State Representative, District 40': ('State House', 40),
    'State Representative, Dist 41': ('State House', 41),
    'State Representative, District 41': ('State House', 41),
    'State Representative, Dist 42': ('State House', 42),
    'State Representative, District 42': ('State House', 42),
    'State Representative, Dist 43': ('State House', 43),
    'State Representative, Dist 44': ('State House', 44),
    'State Representative, Dist 45': ('State House', 45),
    'State Representative, Dist 46': ('State House', 46),
    'State Representative, Dist 47': ('State House', 47),
    'State Representative, Dist 48': ('State House', 48),
    'State Representative, Dist 49': ('State House', 49),
    'State Representative, Dist 50': ('State House', 50),
    'State Representative, Dist 51': ('State House', 51),
    'State Representative, Dist 52': ('State House', 52),
    'State Representative, Dist 53': ('State House', 53),
    'State Representative, Dist 54': ('State House', 54),
    'State Representative, Dist 55': ('State House', 55),
    'State Representative, Dist 56': ('State House', 56),
    'State Representative, District 56': ('State House', 56),
    'State Representative, Dist 57': ('State House', 57),
    'State Representative, Dist 58': ('State House', 58),
    'State Representative, Dist 59': ('State House', 59),
    'State Representative, Dist 60': ('State House', 60),
    'State Representative, Dist 61': ('State House', 61),
    'State Representative, Dist 62': ('State House', 62),
    'State Representative, Dist 63': ('State House', 63),
    'State Representative, Dist 64': ('State House', 64),
    'State Representative, Dist 65': ('State House', 65),
    'State Representative, Dist 66': ('State House', 66),
    'State Representative, Dist 67': ('State House', 67),
    'State Representative, Dist 68': ('State House', 68),
    'State Representative, Dist 69': ('State House', 69),
    'State Representative, Dist 70': ('State House', 70),
    'State Representative, Dist 71': ('State House', 71),
    'State Representative, Dist 72': ('State House', 72),
    'State Representative, Dist 73': ('State House', 73),
    'State Representative, Dist 74': ('State House', 74),
    'State Representative, Dist 75': ('State House', 75),
    'State Representative, Dist 76': ('State House', 76),
    'State Representative, Dist 77': ('State House', 77),
    'State Representative, Dist 78': ('State House', 78),
    'State Representative, Dist 79': ('State House', 79),
    'State Representative, Dist 80': ('State House', 80),
    'State Representative, Dist 81': ('State House', 81),
    'State Representative, Dist 82': ('State House', 82),
    'State Representative, Dist 83': ('State House', 83),
    'State Representative, Dist 84': ('State House', 84),
    'State Representative, Dist 85': ('State House', 85),
    'State Representative, Dist 86': ('State House', 86),
    'State Representative, Dist 87': ('State House', 87),
    'State Representative, Dist 88': ('State House', 88),
    'State Representative, Dist 89': ('State House', 89),
    'State Representative, Dist 90': ('State House', 90),
    'State Representative, Dist 91': ('State House', 91),
    'State Representative, Dist 92': ('State House', 92),
    'State Representative, Dist 93': ('State House', 93),
    'State Representative, Dist 94': ('State House', 94),
    'State Representative, Dist 95': ('State House', 95),
    'State Representative, Dist 96': ('State House', 96),
    'State Representative, Dist 97': ('State House', 97),
    'State Representative, Dist 98': ('State House', 98),
    'State Representative, Dist 99': ('State House', 99),
    'State Representative, Dist 100': ('State House', 100),
    'State Representative, Dist 101': ('State House', 101),
    'State Representative, Dist 102': ('State House', 102),
    'State Representative, Dist 103': ('State House', 103),
    'State Representative Dist 103': ('State House', 103),
    'State Representative, Dist 104': ('State House', 104),
    'State Representative, Dist 105': ('State House', 105),
    'State Representative Dist 105': ('State House', 105),
    'State Representative, Dist 106': ('State House', 106),
    'State Representative, Dist 107': ('State House', 107),
    'State Representative, Dist 108': ('State House', 108),
    'State Representative, Dist 109': ('State House', 109),
    'State Representative, Dist 110': ('State House', 110),
    'State Representative Dist 110': ('State House', 110),
    'State Representative, Dist 111': ('State House', 111),
    'State Representative Dist 111': ('State House', 111),
    'State Representative, Dist 112': ('State House', 112),
    'State Representative Dist 112': ('State House', 112),
    'State Representative, Dist 113': ('State House', 113),
    'State Representative, Dist 114': ('State House', 114),
    'State Representative Dist 114': ('State House', 114),
    'State Representative, Dist 115': ('State House', 115),
    'State Representative Dist 115': ('State House', 115),
    'State Representative, Dist 116': ('State House', 116),
    'State Representative Dist 116': ('State House', 116),
    'State Representative, Dist 117': ('State House', 117),
    'State Representative, Dist 118': ('State House', 118),
    'State Representative Dist 118': ('State House', 118),
    'State Representative, Dist 119': ('State House', 119),
    'State Representative Dist 119': ('State House', 119),
    'State Representative, Dist 120': ('State House', 120),
    'State Representative Dist 120': ('State House', 120)
}


with open(file, "r") as f:
    csvfile = csv.DictReader(f)
    for row in csvfile:
        try:
            office, district = RAW_OFFICE_TO_OFFICE_AND_DISTRICT[row['Contest'].strip()]
            results.append([county, row['Precinct Name'], office, district, row['Candidate Issue'], row['Party'], row['Provisional Votes'], row['Mail Votes'], row['Early Votes'], row['Election Day Votes'], row['Total Votes']])
        except:
            continue


with open(f"20201103__fl__general__{county.lower().replace('-','_')}__precinct.csv", 'w') as output_file:
    outfile = csv.writer(output_file)
    outfile.writerow(['county','precinct', 'office', 'district', 'candidate', 'party', 'provisional', 'mail', 'early_voting', 'election_day', 'votes'])
    outfile.writerows(results)