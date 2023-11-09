# Cybersecurity_project_casestudies
 Reformat the CVE data, by unnesting the elements and then performing data analysis on the exploited vulnerabilities. 


The National Vulnerability Database (NVD) is the U.S. government repository of standards based vulnerability
management data represented using the Security Content Automation Protocol (SCAP). This data
enables automation of vulnerability management, security measurement, and compliance. The NVD includes
databases of security checklist references, security-related software flaws, misconfigurations, product names,
and impact metrics.
The NVD has a fairly easy to use API for developers. See https://nvd.nist.gov/developers/start-here. It
allows for developers to access data on Common Vulnerabilities and Exposures (CVE). “The Common
Vulnerabilities and Exposures (CVE) Program’s primary purpose is to uniquely identify vulnerabilities and
to associate specific versions of code bases (e.g., software and shared libraries) to those vulnerabilities. The
use of CVEs ensures that two or more parties can confidently refer to a CVE identifier (ID) when discussing
or sharing information about a unique vulnerability.” (https://nvd.nist.gov/vuln)
The website exploit-db.com has database contain information about vulnerabilities that have been exploited.
“The Exploit Database is a CVE compliant archive of public exploits and corresponding vulnerable software,
developed for use by penetration testers and vulnerability researchers. Our aim is to serve the most comprehensive
collection of exploits gathered through direct submissions, mailing lists, as well as other public
sources, and present them in a freely-available and easy-to-navigate database. The Exploit Database is a
repository for exploits and proof-of-concepts rather than advisories, making it a valuable resource for those
who need actionable data right away.” (https://www.exploit-db.com/about-exploit-db)
All data, and code used to get the CVE data, is available on GitHub. The CVE data could only be download
2,000 vulnerabilities at a time. These are in separate .rds files in the rawdata folder. The exploit-db data was
obtained from their GitLab site https://gitlab.com/exploit-database/exploitdb/-/blob/main/files_exploits.
csv?ref_type=heads. This .csv file is also in the rawdata folder.
The goal of this part of the case study is to combine data sets from these sources. You will
1. Reformat the CVE data, which is currently in list format, to a data frame
2. Use the exploit-db.com data to create a new column in the CVE data that indicates whether or not
the vulnerability has been exploited.
This data will then be analyzed in Part 2 of the case study.
Here are some links with potentially useful information as far as understanding what the fields in the data
mean. These will likely be more useful in Part 2.
• Some fields in the CVE data are described here (Expand “Response” under CVE API): https://nvd.
nist.gov/developers/vulnerabilities
• Info about CWE types. https://nvd.nist.gov/vuln/categories
• Info about cvssData.
– https://www.first.org/cvss/v3.1/specification-document
– https://www.first.org/cvss/cvss-v3.1.xsd


Deliverables
1. clean.cve.data.r. Code that formats the CVE data into a data frame, and the resulting data
2. clean.exploitdb.data.r. Code that cleans the exploit-db data and prepares it to be used to create
a column in the CVE data.
3. create.exploited.column.r. Code that adds a column exploited to the CVE data indicating
whether or not the vulnerability has been exploited.



################### PART 2 ####################################

In part 1 we extract data about vulnerabilities and whether or not they have been exploited. Suppose our supervisor has never seen the data and wants you to give a brief summary of relationships among the vulnerability characteristics and whether or not they have been exploited. The supervisor needs a quick summary for a meeting this afternoon. Perform some data exploration and analysis to help the supervisor understand the data and prepare for the meeting. 

