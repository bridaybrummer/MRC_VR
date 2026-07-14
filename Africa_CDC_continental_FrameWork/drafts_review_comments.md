# Workshop Draft — Reviewer Comments and Tracked Changes

> Extracted: 21 May 2026  
> Source documents: `Africa_CDC_continental_FrameWork/drafts/`  
> Base document: `Mortality surveillance stakeholder workshop for South Africa_Draft1_17092025_Sent.docx`

---

## Reviewer Summaries

### Mosidi Nhlapo — Statistics South Africa (23 comments)

Mosidi's review is the most substantive and is primarily **corrective of factual inaccuracies** in how her Stats SA presentation was captured. Her comments cluster around five themes:

1. **Framing of Stats SA's role:** The draft overstates Stats SA's barriers and understates its proactive contributions. She objects to language implying Stats SA "restricted" data sharing (it does not; it processes COD data as mandated), that it "attempted to intervene" in collection, and that the CRVS system is "disjointed" (it is manual/paper-based, not disjointed). These passages need to be rewritten or removed.

2. **Coding accuracy:** Stats SA is the *only* institution responsible for mortality coding in South Africa. It does not regard coding as a challenge and has already implemented Iris auto-coding. The draft incorrectly implies improved coding follows automatically from a new capture system — it does not; certification quality is the upstream bottleneck.

3. **ICD-10/11 transition nuance:** Stats SA cannot use DORIS under ICD-10 because DORIS does not include the full code set and cannot handle SA's death volumes; coding is done in batches. This is a capacity/volume issue, not a resistance to modernisation. All countries globally are still in the ICD-10 → ICD-11 transition.

4. **Funeral undertakers:** In current legislation, undertakers are *notifiers*, not certifiers. Certification is done by private doctors or family members (DHA-1680 form). The DHA-1680 is itself a source of poor certification quality. This important nuance is missing from the draft.

5. **Missing Stats SA from D4H:** Stats SA is a partner in the Data for Health (D4H) initiative and should be listed alongside Home Affairs, NICD, and SAMRC.

6. **Minor factual corrections:** NICD's role is not accommodated in the responsibilities table; the "sent to Stats SA" language should be "collected by Stats SA from DHA Head Office"; the 60% automated/40% manual figure needs attribution or removal; pilot data should include timeframes and per-facility breakdowns.

---

### Nadine Nannan — SAMRC (5 substantive comments + 29 tracked changes)

Nadine's review is primarily **editorial and structural**, with specific factual clarifications:

1. **D4H focus areas need renaming:** The four D4H pillars are incorrectly named. They should be: (1) CRVS Governance and Stakeholder Coordination, (2) Notification and Registration of Births and Deaths, (3) Facility-Based COD Data, (4) Production & Use of Vital Statistics.

2. **"Foundational coverage" is undefined:** The term is unclear and possibly incorrect. Death registration coverage should be described as the % of the population with functional access to civil registration services. She flags a gap: no geomapping data exists on DHA office locations vs population density — this is an unresolved analytical challenge.

3. **Leadership clarity needed:** The statement "clear leadership has been identified within health structures" must be made specific — name the lead unit/directorate, or remove the claim.

4. **TWG history:** "Reconstituting" implies a Technical Working Group previously existed. It likely did not. Change to "establishing" or "constituting" a new TWG.

5. **SAMRC's legal status:** SAMRC is a statutory body, not a "government entity." Describe it as "acting on behalf of government" rather than as a government institution.

6. **Strategic Plan section is incomplete:** The appendix C strategic plan is a list of tasks, not a strategic document. It needs high-level stakeholder engagement to be distilled into a proper strategic framework.

**Tracked changes summary:** Nadine rewrote the D4H paragraph — replacing "The Data for Health (D4H) program/initiative, which was launched in May 2020..." with a revised description of SAMRC's initiative (launched 2022) to strengthen South Africa's CRVS system, specifying NDoH, Stats SA, and SAMRC as partners. She also refined the business process mapping sentence, replacing "validating accuracy, identifying bottlenecks, and proposing feasible interventions to enhance functionality and data flow" with "checking for accuracy and flow of information and to consider what can be done to address the bottlenecks."

---

### Natalie Mayet (4 substantive comments + 12 tracked changes)

Natalie's review is **brief but adds missing participants and challenge framing**:

1. **DHA attendance corrected:** Home Affairs *did* have representation during the workshop (later in the week). The introduction should be updated to reflect this rather than only noting their absence.

2. **Missing participants:** USCDC Foundation and Swiss TPH participated in the workshop and should be added to the stakeholder list.

3. **Case fatality rates:** The list of CRVS challenges should include the inability to determine Case Fatality Rates (CFRs) during outbreaks as a direct consequence of poor real-time data.

4. **NDoH analytical capability:** A concern was raised at the workshop about NDoH's analytical capability and data-sharing practices — this should be included in the key challenges or discussion points.

5. **NMC legal passage correction (tracked changes):** The passage stating deaths "could not be added as notifiable conditions" is imprecise. Natalie corrects this: deaths are *not* notifiable under NMC regulations (not diseases/exposures), but maternal deaths *are* already notifiable as a Category 2 condition. The NMC system was not designed for death notification; the passage should reflect this distinction clearly.

---

## AI Agent Implementation Prompt

The following prompt is ready to paste directly into a new agent conversation. The agent should have access to `projects/africa_cdc_workshop/index.qmd`.

---

```
You are editing the Quarto document at:
  projects/africa_cdc_workshop/index.qmd

This is a workshop report for the Africa CDC Mortality Surveillance Stakeholder 
Workshop (28 July–01 August 2025). Apply ALL of the following changes precisely.
Do not add new sections not listed here. Do not alter the overall document 
structure, headings, or YAML frontmatter unless explicitly instructed below.

────────────────────────────────────────────────────────────────
BLOCK A — Factual corrections from Mosidi Nhlapo (Stats SA)
────────────────────────────────────────────────────────────────

A1. SAMRC sentence placement
In the Opening Remarks section, the sentence beginning 
"SAMRC, further reiterated its commitment..." appears inside a paragraph 
describing Stats SA's remarks. Move this sentence to the SAMRC bullet/paragraph 
in the same section so it is correctly attributed.

A2. Stats SA data access language
Find the phrase "and restrict access to vital statistics" (in the CRVS challenges
table or nearby text). Replace with "delayed access to vital statistics" — Stats 
SA publishes data on its website as soon as processing is complete; the problem 
is delay, not restricted access.

A3. DHA-1663 certification quality
Find the phrase "limitations in the DHA-1663 death certification process."
Replace with: "poor quality of medical certification, including deaths certified 
at health facilities and those where the DHA-1663 form is completed without 
adequate clinical information."

A4. Coding dependency on certification
Find text near "improved cause-of-death coding" in the CRVS challenges section.
Add a clarifying note: coding quality is determined upstream by the quality of 
medical certification; improved coding does not follow automatically from a new 
data capture system.

A5. Funeral undertakers — legislative role
Find the sentence: "often registered by funeral undertakers without proper 
medical certification, compromise data accuracy and completeness."
Replace with: "Community deaths are often registered through funeral undertakers 
who, under current legislation, act as *notifiers* on behalf of families — not 
as certifiers. Certification in such cases is performed by private doctors, or 
by a family member using the DHA-1680 form. The DHA-1680 is itself a recognised 
source of poor certification quality."

A6. CRVS system language
Find the word "disjointed" describing the CRVS system. Replace with 
"manual and paper-based" — the system is not structurally disjointed but relies 
heavily on manual processes that introduce delays and inconsistencies.

A7. Stats SA "intervention attempt" passage
Find the passage: "Stats SA attempted to intervene in the data collection process
to address delays and improve data quality efforts that met with resistance from 
various institutions."
Replace with: "Stats SA has proposed possible interventions to fast-track the 
movement of death notification forms from DHA to Stats SA for processing."

A8. ICD-10/11 and DORIS passage
Find the "Data Coding Challenges" bullet or discussion point stating Stats SA 
uses ICD-10 and does not use DORIS. Expand to add:
"Stats SA does not use DORIS because it does not include a full code set and 
cannot handle South Africa's death volumes. Coding is processed in batches 
through IRIS due to capacity constraints. Globally, all countries currently 
publishing mortality statistics from civil registration remain in the ICD-10 to 
ICD-11 transition phase. South Africa is following the same trajectory, with 
Stats SA implementing a phased transition over three years."

A9. Stats SA auto-coding — correct framing of "Data Coding Challenges"
In the "Data Coding Challenges" heading/section, remove or reframe language that 
implies coding is a challenge unique to Stats SA. Add: "Stats SA has implemented 
automated ICD coding through the Iris system, improving both the speed and 
quality of cause-of-death coding. Stats SA is the sole institution in South 
Africa responsible for mortality coding from civil registration."

A10. Stats SA in D4H
Find the sentence: "The D4H initiative focuses on four main areas..." and ensure 
that Stats SA is listed as a founding partner alongside Home Affairs, NICD, and 
SAMRC: "The Data for Health (D4H) initiative was launched in partnership among 
the Department of Home Affairs, NICD, Stats SA, and the South African Medical 
Research Council (SAMRC)."

A11. D4H four focus areas — rename
Replace the four D4H bullet-point labels with the correct names:
  1. CRVS Governance and Stakeholder Coordination
  2. Notification and Registration of Births and Deaths
  3. Facility-Based COD Data
  4. Production and Use of Vital Statistics

A12. "Sent to Stats SA" → "Collected by Stats SA"
Find "sent to Statistics SA" or "sent to Stats SA" in the business process 
description. Replace with "collected by Stats SA from DHA Head Office."

A13. NICD in stakeholder responsibilities table
In the stakeholder responsibilities table (Section: Defined Stakeholder 
Responsibilities), add a row for NICD:
  | NICD | Communicable disease surveillance; early warning integration; 
    technical support for outbreak-related mortality |

A14. NSS alignment clarification
Find "Stats SA has initiated intersectoral collaboration through the NSS 
framework to improve data access and promote alignment of mortality indicators 
across departments." Expand to: "...alignment of mortality indicators across 
departments, consistent with both national and international reporting 
requirements."

A15. Remove or flag the 60% automated / 40% manual figure
Find "proposing a 60% automated and 40% manual approach". Add a footnote or 
inline note: "[Attribution to be confirmed — source of this figure unclear; 
verify with Stats SA before publication.]"

────────────────────────────────────────────────────────────────
BLOCK B — Corrections from Nadine Nannan (SAMRC)
────────────────────────────────────────────────────────────────

B1. D4H paragraph rewrite
Find the paragraph beginning "Another significant initiative is the Data for 
Health (D4H)..." Replace the full paragraph with:

"The SAMRC, in collaboration with the National Department of Health, Statistics 
South Africa, and other partners, has led a programme since 2022 to strengthen 
South Africa's CRVS system. This initiative — the Data for Health (D4H) 
programme — focuses on four pillars: (1) CRVS Governance and Stakeholder 
Coordination, (2) Notification and Registration of Births and Deaths, 
(3) Facility-Based COD Data, and (4) Production and Use of Vital Statistics. 
Governance activities include quarterly vital statistics meetings ongoing since 
2017. Process improvement efforts aim to reduce backlogs and revise data 
workflows. Capacity building has involved training for doctors on MCCD and ICD 
coding. The business process mapping component examines the death registration 
pathway across community and facility settings, checking for accuracy and flow 
of information and identifying what can be done to address bottlenecks."

B2. "Foundational coverage" — replace with correct definition
Find "foundational coverage" and replace with:
"civil registration coverage, measured as the proportion of the population with 
functional access to civil registration services. The geographic distribution of 
DHA offices relative to population density remains an important gap in 
understanding whether access is equitable across the country."

B3. Leadership clarity
Find "This is now resolved, with clear leadership identified within health 
structures." Replace with:
"This is now resolved, with the Directorate of Epidemiology and Surveillance 
within the National Department of Health identified as the lead for national 
mortality surveillance coordination."
(If the specific directorate name cannot be confirmed, flag with a TODO callout 
instead of removing.)

B4. TWG — replace "Reconstituting" with "Establishing"
Find "Reconstituting the Technical Working Group (TWG)". Replace with 
"Establishing a new Technical Working Group (TWG)" — no prior TWG is known to 
have existed.

B5. SAMRC legal status
Find "government entities" in the Sub-Committee membership table where SAMRC 
appears. Change the row label from "Government entities" to 
"Statutory bodies / public entities" and update the description to: 
"SAMRC acts on behalf of government as a statutory body established to improve 
health through research; NICD is a division of the National Health Laboratory 
Service (NHLS)."

B6. Strategic Plan — flag as incomplete
In Section: Strategic Plan Framework (or Appendix C), add a callout box:
`::: {.callout-warning title="Work in progress"}
The strategic plan draft currently contains a high-level list of activities. 
It requires further engagement and distillation by senior stakeholders from 
NDoH, Stats SA, DHA, SAMRC, and NICD before it can function as a strategic 
document. A dedicated session is recommended to refine the vision, objectives, 
and phased activities into a coherent strategic framework.
:::`

────────────────────────────────────────────────────────────────
BLOCK C — Corrections from Natalie Mayet
────────────────────────────────────────────────────────────────

C1. DHA attendance — Day 1 Introduction
Find: "The Department of Home Affairs was unable to attend Day 1 but its central 
role in mortality registration was acknowledged..."
Replace with: "The Department of Home Affairs was unable to attend on Day 1; 
however, a DHA representative joined from Day 2 onwards, completing the 
stakeholder group for the remainder of the workshop."
(Remove the duplicate mention of DHA joining Day 2 that already exists in the 
Day 2 section, or make Day 2 reference a brief cross-reference.)

C2. Add missing participants
In the Introduction or Stakeholder Engagement section, find the stakeholder list. 
Add after "Africa CDC":
"The workshop also received technical contributions from the USCDC Foundation 
and Swiss TPH, who shared international lessons from mortality surveillance 
system strengthening in other country contexts."

C3. Add CFR challenge to CRVS challenges table
In the CRVS challenges table, add a new row:
  | **Outbreak response limitations** | The absence of timely cause-specific 
    mortality data prevents the calculation of Case Fatality Rates (CFRs) during 
    disease outbreaks, directly limiting the ability to characterise outbreak 
    severity and guide response. |

C4. NDoH analytical capability concern
In the Day 1 Key Discussion Points section (under NDoH presentation or Workshop 
Objectives), add a new discussion point:
"**8. NDoH Analytical Capability and Data Sharing**  
Concerns were raised about the current analytical capability within NDoH and the 
extent to which mortality data is shared across departments and with the public 
health community. Strengthening in-house analytical capacity was identified as a 
prerequisite for NDoH to fulfil its proposed surveillance lead role."

C5. NMC legal passage — correct framing
Find the passage: "legal advice confirmed that deaths could not be added as 
notifiable conditions under the current definitions, making notification through 
the NMC system unfeasible."
Replace with:
"Legal advice confirmed that deaths, as events, do not fall within the definition 
of notifiable medical conditions under current NMC regulations — which apply to 
diseases and exposures, not deaths per se. However, it should be noted that 
maternal deaths are already notifiable as a Category 2 condition. The NMC 
surveillance system was not designed for routine death notification; an 
alternative legislative mechanism was therefore required."

C6. Add MRC and NICD to SAMRC presentation credits
In the SAMRC presentation section header or introductory sentence, ensure both 
SAMRC/MRC and NICD are acknowledged as co-presenters or contributing 
institutions where appropriate.

────────────────────────────────────────────────────────────────
FINAL CHECKS
────────────────────────────────────────────────────────────────

After applying all changes:
1. Ensure all callout boxes use valid Quarto syntax (`::: {.callout-*}`).
2. Ensure all table rows are valid Markdown pipe-table syntax.
3. Do not remove any existing TODO or feedback callout boxes already in the 
   document.
4. Do not alter the YAML frontmatter, section anchors, or section numbering.
5. Validate that the document still renders without errors by checking for 
   unclosed fences or broken table rows.
```

---

## StatsSA Comments (v2)

**Primary reviewer:** Stats SA reviewer  
**Comments:** 23  
**Tracked changes (insertions + deletions):** 0

### Comments

#### Comment 1 — Mosidi Nhlapo (2025-10-14)

> **Commented on:** *"SAMRC, further reiterated its commitment to providing technical expertise and capacity to ensure the development of a sustainable, reliable, and responsive mortality surveillance system for South Africa."*

Does this belong in this paragraph?

#### Comment 2 — Mosidi Nhlapo (2025-10-15)

> **Commented on:** *"and restrict access to vital statistics."*

Stats SA makes data available to all users on the website as soon as data processing cycle is completed. This issue was corrected during the meeting “Rather talk to  delayed access to data”

#### Comment 3 — Mosidi Nhlapo (2025-10-16)

> **Commented on:** *"limitations in the DHA-1663 death certification process."*

This should point to poor quality of certification , including those emanating from health facilities

#### Comment 4 — Mosidi Nhlapo (2025-10-14)

> **Commented on:** *"improved cause-of-death coding,"*

Coding depends on proper certification of causes of deaths. Improved coding does not happen because there is a new system of capturing causes of death.

#### Comment 5 — Mosidi Nhlapo (2025-10-15)

> **Commented on:** *"often registered by funeral undertakers without proper medical certification, compromise data accuracy and completeness."*

In the current legislation funeral undertakers are notifiers and this role is realised where the family has given permission to register on their behalf  The certification in such cases is done mainly private doctors or by the family member in the case of DHA - 1680. The DHA-1680 are also the source of poor certification

#### Comment 6 — Mosidi Nhlapo (2025-10-16)

> **Commented on:** *"Pilot visits have already been conducted at KPTH and SBAH. As an example of expected outputs, 68 causes of death have been identified using ICD-11 codes."*

It will be ideal to include the time frame for these activities and break it down by facility indicating how much was captured per facility, over what period

#### Comment 7 — Mosidi Nhlapo (2025-10-16)

> **Commented on:** *". Data Coding Challenges"*

Stats SA is currently the only institution responsible for mortality coding in South Africa and does not regard coding as a challengeAs indicated during my talk at the workshop, Stats SA has implemented auto coding, this has improved the quality and speed of coding This sentence needs to be reviewed

#### Comment 8 — Mosidi Nhlapo (2025-10-14)

> **Commented on:** *"STATS SA still uses ICD- 10 and they do not use DORIS, while DHIS2 uses ICD- 11 and DORIS."*

It must be noted that all countries that are currently publishing mortality and causes of deaths from civil registration globally, are still in the transition phase from ICD10 to ICD11.South Africa is also still in the transition phase, Stats SA does not use Doris (even under ICD10) because it does not include a full list of codes and cannot handle the total volume of South African deaths. Even in the current environment of IRIS, coding is done in batches due to the capacity of full IRIS

#### Comment 9 — Mosidi Nhlapo (2025-10-14)

> **Commented on:** *"the disjointed processes involving the death notification form, which remains managed by the Department of Home Affairs"*

This does not reflect Stats SA commentsStats SA comments were that … The current system is paper based which requires manual intervention and thus time consuming.The wording in this sentence needs to be reviewed, the CR system in South Africa is not disjointed, it is manual

#### Comment 10 — Mosidi Nhlapo (2025-10-14)

> **Commented on:** *"where Stats SA attempted to intervene in the data collection process to address delays and improve data quality efforts that met with resistance from various institutions."*

It does not reflect Stats SA commentsPlease note the comments above on the role of funeral undertakers in the CRVS system The example was made of a proposed possible intervention to fast track the movement of forms

#### Comment 11 — Mosidi Nhlapo (2025-10-14)

> **Commented on:** *"Stats SA has implemented several initiatives, including automated form tracking, the introduction of unique identifiers, and internal coordination to reduce data entry duplication and errors."*

This comment was on the data processing process within Stats SA, this is not a certification quality intervention

#### Comment 12 — Mosidi Nhlapo (2025-10-15)

> **Commented on:** *"Concerns were raised regarding the lack of integration and feedback mechanisms across departments and systems, which result in significant delays and duplication in reporting processes."*

This was not part of my talk

#### Comment 13 — Mosidi Nhlapo (2025-10-15)

> **Commented on:** *"Additionally, Stats SA has initiated intersectoral collaboration through the NSS framework to improve data access and promote alignment of mortality indicators across departments."*

The speech talked to alignment of all indicators for both national and international reporting

#### Comment 14 — Mosidi Nhlapo (2025-11-14)

> **Commented on:** *"Additionally, Stats SA confirmed progress in automation, proposing a 60% automated and 40% manual approach"*

We are not sure what this relates to

#### Comment 15 — Mosidi Nhlapo (2025-10-15)

> **Commented on:** *"Another significant initiative is the Data for Health (D4H) program, which was launched in May 2020 through a partnership among Home Affairs, NICD, and the South African Medical Research Council (SAMRC)."*

Stats SA is also part of the D4H initiative in South Africa

#### Comment 16 — Mosidi Nhlapo (2025-11-14)

> **Commented on:** *"data quality issues, and lack of traceability remain key limitations."*

Please clarify what this paragraph relates to

#### Comment 17 — Mosidi Nhlapo (2025-10-15)

> **Commented on:** *"Department of Health : Lead implementation"*

How is NICD accommodated?

#### Comment 18 — Mosidi Nhlapo (2025-10-15)

> **Commented on:** *"and sent to Statistics SA"*

Should read…...And collected by Stats SA from DHA Head Office

#### Comment 19 — Mosidi Nhlapo (2025-10-15)

> **Commented on:** *"As an example, Stats SA needs assistance in improving the records for births and deaths at satellite offices that are located inside health facilities."*

In the discussions it was said that DHA needs assistance with timely birth and death registrations Stats SA needs assistance in improving the certification of causes of death by medical practitioners …. Please review

#### Comment 20 — Mosidi Nhlapo (2025-10-15)

> **Commented on:** *"The discussion also acknowledged that Stats SA had previously restricted direct data sharing between Home Affairs and the Department of Health, underscoring the critical role of interdepartmental legislative agreements"*

What is the basis of this sentence? This is not correct reflection of our discussionsStats SA’s mandate is to process the causes of death from civil registrations Please review

#### Comment 21 — Brian Brummer (2025-08-04)

Aphiwe Dinga to include

#### Comment 22 — Brian Brummer (2025-09-10)

We don't yet have a ToR for mortality surveillance. Ask Mbhekiseni to work with Aphiwe DInga.

#### Comment 23 — Brian Brummer (2025-08-04)

Aphiwe Dinga to include

*No tracked changes found.*

---

## NN Comments

**Primary reviewer:** Nadine Nannan (SAMRC)  
**Comments:** 11  
**Tracked changes (insertions + deletions):** 29

### Comments

#### Comment 1 — Nadine Nannan | SAMRC (2025-10-17)

> **Commented on:** *"The D4H initiative focuses on four main areas: governance, process improvement, capacity building, and business process mapping."*

1. CRVS GOVERNANCE AND STAKEHOLDER COORDINATION2. NOTIFICATION AND REGISTRATION OF BIRTHS AND DEATHS3. FACILITY-BASED COD DATA4. PRODUCTION & USE OF VITAL STATISTICS

#### Comment 2 — Nadine Nannan | SAMRC (2025-10-17)

> **Commented on:** *"foundational coverage"*

Not sure what is meant by foundational coverage? Coverage is measured as the % of population living in areas where CRS is functional. We know that DHA offices accept death registrations across the country but there is no information regarding the spread of the offices and whether functional access is equitable. We need geomapping information about DHA offices and corresponding population density to better understand the criteria of coverage.  I would say, that is the challenge.

#### Comment 3 — Nadine Nannan | SAMRC (2025-10-17)

> **Commented on:** *"This is now resolved, with clear leadership identified within health structures."*

If this is so, please define this here, rather than just saying Health.

#### Comment 4 — Nadine Nannan | SAMRC (2025-10-17)

> **Commented on:** *"Reconstituting"*

Reconstituting implies a TWG existed in the past. I don’t think that any TWG previously existed.

#### Comment 5 — Nadine Nannan | SAMRC (2025-10-17)

> **Commented on:** *"(SAMRC"*

I am not sure that the MRC is a government entity. The MRC is a statutory body established to improve health through research. On behalf of government, rather than of government?

#### Comment 6 — Brian Brummer (2025-08-04)

Aphiwe Dinga to include

#### Comment 7 — Brian Brummer (2025-09-10)

We don't yet have a ToR for mortality surveillance. Ask Mbhekiseni to work with Aphiwe DInga.

#### Comment 8 — Brian Brummer (2025-08-04)

Aphiwe Dinga to include

#### Comment 9 — Nadine Nannan | SAMRC (2025-10-17)

> **Commented on:** *"Strategic Plan Draft"*

This is an important section, but at the moment is incomplete. It is a list of things to do. The activities need to be thought through and distilled. As a strategic document it needs high-level engagement from the stakeholders involved.

#### Comment 10 — Brian Brummer (2025-08-04)

Aphiwe Dinga to include

#### Comment 11 — Brian Brummer (2025-09-10)

Brian actually has this

### Tracked Changes

#### Change 1 — Insertion by Nadine Nannan | SAMRC (2025-10-17)

**Added:** `development`

#### Change 2 — Insertion by Nadine Nannan | SAMRC (2025-10-17)

**Added:** `initiative`

#### Change 3 — Insertion by Nadine Nannan | SAMRC (2025-10-17)

**Added:** `to strengthen South Africa’s CRVS system`

#### Change 4 — Insertion by Nadine Nannan | SAMRC (2025-10-17)

**Added:** `2022`

#### Change 5 — Insertion by Nadine Nannan | SAMRC (2025-10-17)

**Added:** `National Department of`

#### Change 6 — Insertion by Nadine Nannan | SAMRC (2025-10-17)

**Added:** `Health, Statistics`

#### Change 7 — Insertion by Nadine Nannan | SAMRC (2025-10-17)

**Added:** `South Africa,`

#### Change 8 — Insertion by Nadine Nannan | SAMRC (2025-10-17)

**Added:** `,`

#### Change 9 — Insertion by Nadine Nannan | SAMRC (2025-10-17)

**Added:** `initially`

#### Change 10 — Insertion by Nadine Nannan | SAMRC (2025-10-17)

**Added:** `issues of`

#### Change 11 — Insertion by Nadine Nannan | SAMRC (2025-10-17)

**Added:** `Health, MRC and`

#### Change 12 — Insertion by Nadine Nannan | SAMRC (2025-10-17)

**Added:** `timely`

#### Change 13 — Insertion by Nadine Nannan | SAMRC (2025-10-17)

**Added:** `research,`

#### Change 14 — Insertion by Nadine Nannan | SAMRC (2025-10-17)

**Added:** `public health`

#### Change 15 — Insertion by Nadine Nannan | SAMRC (2025-10-17)

**Added:** `s`

#### Change 16 — Insertion by Nadine Nannan | SAMRC (2025-10-17)

**Added:** `and checking for`

#### Change 17 — Insertion by Nadine Nannan | SAMRC (2025-10-17)

**Added:** `accuracy`

#### Change 18 — Insertion by Nadine Nannan | SAMRC (2025-10-17)

**Added:** `,`

#### Change 19 — Insertion by Nadine Nannan | SAMRC (2025-10-17)

**Added:** `flow of information`

#### Change 20 — Insertion by Nadine Nannan | SAMRC (2025-10-17)

**Added:** `and to`

#### Change 21 — Insertion by Nadine Nannan | SAMRC (2025-10-17)

**Added:** `consider what can be done to address the bottlenecks`

#### Change 22 — Insertion by Nadine Nannan | SAMRC (2025-10-17)

**Added:** `.`

#### Change 23 — Deletion by Nadine Nannan | SAMRC (2025-10-17)

**Deleted:** ~~`initiative`~~

#### Change 24 — Deletion by Nadine Nannan | SAMRC (2025-10-17)

**Deleted:** ~~`program,`~~

#### Change 25 — Deletion by Nadine Nannan | SAMRC (2025-10-17)

**Deleted:** ~~`which was`~~

#### Change 26 — Deletion by Nadine Nannan | SAMRC (2025-10-17)

**Deleted:** ~~`2020`~~

#### Change 27 — Deletion by Nadine Nannan | SAMRC (2025-10-17)

**Deleted:** ~~`hospitals to`~~

#### Change 28 — Deletion by Nadine Nannan | SAMRC (2025-10-17)

**Deleted:** ~~`relevant`~~

#### Change 29 — Deletion by Nadine Nannan | SAMRC (2025-10-17)

**Deleted:** ~~`validating its accuracy, identifying bottlenecks, and proposing feasible interventions to enhance the system’s functionality and data flow`~~

---

## Natalie Comments

**Primary reviewer:** Natalie Mayet  
**Comments:** 10  
**Tracked changes (insertions + deletions):** 12

### Comments

#### Comment 1 — Natalie Mayet (2025-10-22)

> **Commented on:** *"Affairs"*

Home Affairs did have representation later

#### Comment 2 — Natalie Mayet (2025-10-22)

> **Commented on:** *"which"*

? Add participation from USCDC Foundation and Swiss TPH

#### Comment 3 — Natalie Mayet (2025-10-22)

> **Commented on:** *"completeness"*

Add the inability to determine Case Fatality rates during outbreaks

#### Comment 4 — Natalie Mayet (2025-10-22)

> **Commented on:** *"priorities"*

A concern was also expressed about the analytical capability of NDOH and sharing of data

#### Comment 5 — Brian Brummer (2025-08-04)

Aphiwe Dinga to include

#### Comment 6 — Brian Brummer (2025-09-10)

We don't yet have a ToR for mortality surveillance. Ask Mbhekiseni to work with Aphiwe DInga.

#### Comment 7 — Brian Brummer (2025-08-04)

Aphiwe Dinga to include

#### Comment 8 — Natalie Mayet (2025-10-22)

> **Commented on:** *"SA"*

MRC, NICD ?

#### Comment 9 — Brian Brummer (2025-08-04)

Aphiwe Dinga to include

#### Comment 10 — Brian Brummer (2025-09-10)

Brian actually has this

### Tracked Changes

#### Change 1 — Insertion by Natalie Mayet (2025-10-22)

**Added:** `exposure.`

#### Change 2 — Insertion by Natalie Mayet (2025-10-22)

**Added:** `are not a`

#### Change 3 — Insertion by Natalie Mayet (2025-10-22)

**Added:** `“`

#### Change 4 — Insertion by Natalie Mayet (2025-10-22)

**Added:** `”`

#### Change 5 — Insertion by Natalie Mayet (2025-10-22)

**Added:** `regulations`

#### Change 6 — Insertion by Natalie Mayet (2025-10-22)

**Added:** `eve`

#### Change 7 — Insertion by Natalie Mayet (2025-10-22)

**Added:** `n though maternal deaths are notifiable as a category 2 condition`

#### Change 8 — Deletion by Natalie Mayet (2025-10-22)

**Deleted:** ~~`outbreak.`~~

#### Change 9 — Deletion by Natalie Mayet (2025-10-22)

**Deleted:** ~~`within the Notifiable Medical Condition (NMC) Surveillance System`~~

#### Change 10 — Deletion by Natalie Mayet (2025-10-22)

**Deleted:** ~~`could not be added as`~~

#### Change 11 — Deletion by Natalie Mayet (2025-10-22)

**Deleted:** ~~`s`~~

#### Change 12 — Deletion by Natalie Mayet (2025-10-22)

**Deleted:** ~~`definitions`~~

---
