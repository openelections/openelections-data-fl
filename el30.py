import csv

source = '/Users/derekwillis/code/openelections-sources-fl/2020/general/Broward FL EL30-Results-By-Precinct.txt'
offices = ['Straight Party', 'President and Vice President', 'Representative in Congress - District 22', 'Representative in Congress - District 23', 'Representative in Congress - District 24',
'State Senator - District 29', 'State Senator - District 35', 'State Representative - District 92', 'State Representative - District 93', 'State Representative - District 96', 'State Representative - District 101',
'State Representative - District 103', 'State Representative - District 104', 'State Representative - District 105', 'State Attorney - 17th Judicial Circuit', 'Public Defender - 17th Judicial Circuit',
'Sheriff', 'Supervisor of Elections', 'County Commissioner - District 7', 'County Commissioner - District 9', 'Justice of the Supreme Court - Carlos G. Muniz', 'Fourth District Court of Appeal - Alan O. Forst',
'Fourth District Court of Appeal - Mark W. Klingensmith', 'Fourth District Court of Appeal - Martha C. Warner', 'Circuit Judge - 17th Judicial Circuit - Group 16', 'School Board At Large - Seat 9',
'Broward Soil and Water Conservation District - Seat 5', 'No. 1 Constitutional Amendment', 'No. 2 Constitutional Amendment', 'No. 3 Constitutional Amendment', 'No. 4 Constitutional Amendment', 'No. 5 Constitutional Amendment',
'No. 6 Constitutional Amendment', 'Broward County Ballot Question', 'Broward County Charter Question', 'City Commissioner Seat 3 Coral Springs', 'City Commissioner Seat 5 Coral Springs', 'City Commissioner Dania Beach',
'Mayor Fort Lauderdale', 'City Commissioner District 2 Fort Lauderdale', 'Mayor At Large Hallandale Beach', 'City Commission Seat 3 Hallandale Beach', 'City Commission Seat 4 Hallandale Beach', 'Commissioner - District 2 Hollywood',
'Commissioner - District 4 Hollywood', 'Commissioner - District 6 Hollywood', 'City Commissioner Seat 1 Margate', 'City Commissioner Seat 2 Margate', 'City Commissioner Seat 4 Margate', 'City Commissioner District C North Lauderdale',
'City Commission Oakland Park', 'Mayor At Large Parkland', 'City Commission District 1 Parkland', 'City Commission District 2 Parkland', 'City Commission District 4 Parkland', 'City Council Group 3 Plantation', 'City Council Group 4 Plantation',
'City Council Group 5 Plantation', 'Mayor Pompano Beach', 'City Commissioner District 1 Pompano Beach', 'City Commissioner District 2 Pompano Beach', 'City Commissioner District 3 Pompano Beach', 'City Commissioner District 4 Pompano Beach',
'City Commissioner District 5 Pompano Beach', 'Mayor Southwest Ranches', 'Council Member District 3 Southwest Ranches', 'Council Member District 4 Southwest Ranches', 'City Commissioner District 1 Tamarac',
'City Commissioner District 3 Tamarac', 'Mayor West Park', 'City Commissioner Seat 3 West Park', "City Commissioner Seat 4 West Park", "Mayor Weston", "City Commissioner Seat 1 Weston", "City Commissioner Seat 2 Weston", "Mayor Wilton Manors", "Commissioner Wilton Manors", "Cypress Cove Community Development District - Seat 5", "Griffin Lakes Community Development District - Seat 5", "Walnut Creek Community Development District - Seat 5", "Coconut Creek Question 1", "Coconut Creek Question 2", "Coconut Creek Question 3", "Coconut Creek Question 4", "Coconut Creek Question 5", "Coconut Creek Question 6", "Coconut Creek Question 7", "Coconut Creek Question 8", "Coconut Creek Question 9", "Coconut Creek Question 10", "Coconut Creek Question 11", "Coconut Creek Question 12", "Coconut Creek Question 13", "Coconut Creek Question 14", "Coconut Creek Question 15", "Coconut Creek Question 16", "Coconut Creek Question 17", "Coconut Creek Question 18", "Mayor Cooper City", "City Commission District 4 Cooper City", "Cooper City Question 1", "Cooper City Question 2", "Cooper City Question 3", "Cooper City Question 4", "Cooper City Question 5", "Cooper City Question 6", "Cooper City Question 7", "Cooper City Question 8", "Cooper City Question 9", "Cooper City Question 10", "Cooper City Question 11", "Davie Question 1", "Davie Question 2", "Davie Question 3", "Davie Question 4", "Davie Question 5", "Davie Question 6", "Davie Question 7", "Davie Question 8", "Davie Question 9", "Davie Question 10", "Davie Question 11", "Commissioner Seat 4 Lauderdale Lakes", "Lauderdale Lakes Question", "Seat 1 Commissioner Lauderhill", "Seat 2 Commissioner Lauderhill", "Seat 3 Commissioner Lauderhill", "Lauderhill Question", 'Commissioner Group "D" Sunrise', "Sunrise Ballot Question 1", "Sunrise Ballot Question 2", "Sunrise Ballot Question 3"]

lines = open(source).readlines()
results = []

for line in lines:
    if line == '\n':
        continue
    if line == 'DISTRICT\n':
        continue
    if "<" in line:
        continue
    if "PREC REPORT-GROUP DETAIL" in line:
        continue
    if "General Election" in line:
        continue
    if "November 3, 2020" in line:
        continue
    if "Broward County, Florida" in line:
        continue
    if "Report EL30A" in line:
        continue
    if "Run Date" in line:
        precinct = None
        continue
    if "TOTAL VOTES" in line:
        continue
    if '(VOTE FOR)' in line:
        continue
    if 'VOTER TURNOUT - TOTAL' in line:
        continue
    if any(o in line for o in offices):
        office = line.strip()
    if not ".  ." in line and not any(o in line for o in offices) and line[0] == '0':
        precinct = line.strip()
    if ".  ." in line:
        if "REGISTERED VOTERS" in line:
            candidate = None
            office = "Registered Voters"
            party = None
            votes = line.split('.  .', 1)[1].split(' ',1)[1].replace('.','').strip()
            election_day, absentee, early_votes, provisional = ["", "", "", ""]
        elif "BALLOTS CAST - BLANK" in line:
            office = "Ballots Cast Blank"
            candidate = None
            party = None
            if len([x.strip() for x in line.split('.  .', 1)[1].split(' ',1)[1].split('   ') if x !='']) == 5:
                fill, votes, election_day, absentee, early_votes, provisional = [x.strip() for x in line.split('.  .', 1)[1].split(' ',1)[1].split('   ') if x !='']
            else:
                fill, votes, pct, election_day, absentee, early_votes, provisional = [x.strip() for x in line.split('.  .', 1)[1].split(' ',1)[1].split('   ') if x !='']
        elif "BALLOTS CAST" in line:
            office = "Ballots Cast"
            candidate = None
            party = None
            if len([x.strip() for x in line.split('.  .', 1)[1].split(' ',1)[1].split('   ') if x !='']) == 5:
                fill, votes, election_day, absentee, early_votes, provisional = [x.strip() for x in line.split('.  .', 1)[1].split(' ',1)[1].split('   ') if x !='']
            else:
                fill, votes, election_day, absentee, early_votes, provisional = [x.strip() for x in line.split('.  .', 1)[1].split(' ',1)[1].split('   ') if x !='']
        elif 'WRITE-IN' in line:
            candidate = 'Write-ins'
            party = None
            if len([x.strip() for x in line.split('.  .', 1)[1].split(' ',1)[1].split('   ') if x !='']) == 6:
                fill, votes, election_day, absentee, early_votes, provisional = [x.strip() for x in line.split('.  .', 1)[1].split(' ',1)[1].split('   ') if x !='']
            else:
                fill, votes, pct, election_day, absentee, early_votes, provisional = [x.strip() for x in line.split('.  .', 1)[1].split(' ',1)[1].split('   ') if x !='']
        elif 'Total' in line:
            continue
        elif 'VOTER TURNOUT' in line:
            continue
        elif 'Over Votes' in line:
            candidate = 'Over Votes'
            party = None
            if len([x.strip() for x in line.split('.  .', 1)[1].split(' ',1)[1].split('   ') if x !='']) == 6:
                fill, votes, election_day, absentee, emergency, provisional, federal = [x.strip() for x in line.split('.  .', 1)[1].split(' ',1)[1].split('   ') if x !='']
            else:
                fill, votes, pct, election_day, absentee, emergency, provisional, federal = [x.strip() for x in line.split('.  .', 1)[1].split(' ',1)[1].split('   ') if x !='']
        elif 'Under Votes' in line:
            candidate = 'Under Votes'
            party = None
            if len([x.strip() for x in line.split('.  .', 1)[1].split(' ',1)[1].split('   ') if x !='']) == 6:
                fill, votes, election_day, absentee, emergency, provisional, federal = [x.strip() for x in line.split('.  .', 1)[1].split(' ',1)[1].split('   ') if x !='']
            else:
                fill, votes, pct, election_day, absentee, emergency, provisional, federal = [x.strip() for x in line.split('.  .', 1)[1].split(' ',1)[1].split('   ') if x !='']
        else:
            if '(' in line:
                candidate, party = line.split('(', 1)
                party = party[0:3]
            else:
                candidate = line.split(' .')[0]
                party = None
            candidate = candidate.strip()
            if len([x.strip() for x in line.split('.  .', 1)[1].split(' ',1)[1].split('   ') if x !='']) == 6:
                fill, votes, election_day, absentee, early_votes, provisional = [x.strip() for x in line.split('.  .', 1)[1].split(' ',1)[1].split('   ') if x !='']
            else:
                fill, votes, pct, election_day, absentee, early_votes, provisional = [x.strip() for x in line.split('.  .', 1)[1].split(' ',1)[1].split('   ') if x !='']
        results.append(['Broward', precinct, office, None, party, candidate, votes.replace(',','').strip(), election_day.replace(',','').strip(), absentee.replace(',','').strip(), early_votes.replace(',','').strip(), provisional.replace(',','').strip()])

with open('20201103__fl__general__broward__precinct.csv', 'wt') as csvfile:
    w = csv.writer(csvfile)
    headers = ['county', 'precinct', 'office', 'district', 'party', 'candidate', 'votes', 'election_day', 'absentee', 'early_voting', 'provisional']
    w.writerow(headers)
    w.writerows(results)
