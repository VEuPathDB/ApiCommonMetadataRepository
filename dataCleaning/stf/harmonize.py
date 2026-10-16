#!/usr/bin/env python3
"""Build per-organism harmonized sample tables + STF ontology mapping files.

Sample identity comes from dnaseq_sample_metadata_mapping.txt (already verified),
so every row is traceable back to its metadata source and join evidence.
"""
import csv,os,re,json,glob,collections,warnings,sys,hashlib
warnings.filterwarnings('ignore')
ROOT=os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
REPO_ROOT=os.path.dirname(ROOT)
OUT=f'{ROOT}/stf/harmonized'

# ---------------------------------------------------------------- vocabulary
ONT=json.load(open(f'{ROOT}/stf/ontology_corpus.json'))
CORPUS=ONT['corpus']
def normh(h):
    h=re.sub(r'^(Characteristics|Comment|Parameter Value|Factor Value)\s*\[(.*)\]$',r'\2',str(h).strip())
    return re.sub(r'[^a-z0-9]','',h.lower())
def dataset_id(presenter_name):
    """DS_ presenter id: same digest as DatasetPresenter.getId() in
    EbrcModelCommon -- DigestUtils.sha1Hex(presenter name).substring(0,10).
    A chunked dataset's presenter is a pattern presenter, so this is the digest of
    the presenter's name, never of the chunk's own dataset name."""
    return 'DS_'+hashlib.sha1(presenter_name.encode()).hexdigest()[:10]

def varname(canonical):
    v=re.sub(r'[^A-Za-z0-9]+','_',canonical.strip()).strip('_')
    return re.sub(r'_+','_',v).lower()

# 17 minted terms (approved: mint in the wrangle script, not in ontologyMappings.xml)
MINTED={
 'assay_type':            ('Assay type','Library strategy of the sequencing assay (e.g. WGS, WGA).'),
 'library_layout':        ('Library layout','Whether the sequencing library is SINGLE or PAIRED end.'),
 'library_selection':     ('Library selection','Method used to select or enrich the sequencing library.'),
 'library_source':        ('Library source','Source material type of the sequencing library (e.g. GENOMIC).'),
 'sequencing_platform':   ('Sequencing platform','Sequencing technology platform (e.g. ILLUMINA).'),
 'sequencing_instrument': ('Sequencing instrument','Specific sequencing instrument model.'),
 'average_read_length':   ('Average read length','Mean sequenced read length in bases.'),
 'developmental_stage':   ('Developmental stage','Life-cycle stage of the organism sampled (e.g. oocyst, mycelium).'),
 'ploidy':                ('Ploidy','Number of chromosome sets in the sampled isolate.'),
 'fungal_genotype':       ('Fungal genotype','Genotype designation of the fungal isolate.'),
 'known_pathogenicity':   ('Known pathogenicity','Whether the isolate is known to be pathogenic.'),
 'diagnostic_method':     ('Diagnostic method','Method used to diagnose or detect the organism.'),
 'sample_collection_method':('Sample collection method','Method used to collect the specimen.'),
 'sample_collection_device':('Sample collection device','Device used to collect the specimen.'),
 'collection_site':       ('Collection site','Named site or institution where the specimen was collected.'),
 'parasite_clearance_half_life':('Parasite clearance half-life','Parasite clearance half-life following treatment.'),
 'genome_coverage_breadth_5x':('Genome coverage breadth at 5x','Percent of reference genome covered at >=5x read depth.'),
 # --- 3 extra mints: ISA headers that have no term in ontologyMappings.xml ---
 'state':                 ('State or province','Sub-national administrative region of collection.'),
 'repository_id':         ('Repository ID','Identifier of the isolate in the originating strain repository.'),
 'fingerprint_clade':     ('Fingerprint clade','Clade assignment from DNA fingerprinting.'),
 'culture_condition':     ('Culture condition','Axenic / xenic / monoxenic culture purity of the isolate.'),
 # Deliberately generic: the values here are cities, districts, multi-state
 # regions ('Mid-Atlantic'), woodlands and valleys. ENVO_00000856 is 'city'
 # specifically, so it would be wrong to claim it. Needs a real GAZ/ENVO id.
 'geographic_location':   ('Geographic location','Named place of collection below country/state level - city, district, locality or region.'),
 'collection_month':      ('Collection month','Month of specimen collection (1-12) where known.'),
 'dataset_id':            ('Dataset','Dataset (RSRC) this sample came from, as the DS_ presenter id: "DS_" + first 10 hex chars of sha1(datasetName), matching DatasetPresenter.getId(). Hashed from the NEW dnaseq workflow dataset name.'),
 'sample_display_name':   ('Sample name','Preferred human-readable sample name. sample_id is kept byte-identical to what the dnaseq workflow emitted so it joins to the alignment stats; where the harmonized ApiCommonMetadataRepository name differs, that curated name is shown here instead.'),
 'collecting_institution':('Collecting institution','Institution that collected the specimen, as distinct from the named collector.'),
 'isolation_source':      ('Isolation source','Material or site the organism was isolated from, as distinct from the specimen type.'),
 'collection_date_original':('Collection date as provided','Verbatim collection date string from the data provider, retained because the typed collection_date is null whenever the value is incomplete or ambiguous.'),
 'host_disease_stage':    ('Host disease stage','Stage of the host disease at sampling, as reported in the NCBI BioSample record.'),
 'pulmonary_disorder':    ('Pulmonary disorder','Pulmonary disorder of the host, as reported in the NCBI BioSample record.'),
 'biomaterial_provider':  ('Biomaterial provider','Person or institution that provided the sampled material.'),
 'serotype':              ('Serotype','Serotype of the organism under investigation, as reported in the NCBI BioSample record.'),
 'trichothecene_chemotype':('Trichothecene chemotype','Fusarium trichothecene mycotoxin chemotype, such as 3ADON or NIV.'),
}
# ISA headers with no ontologyMappings.xml term -> minted variable (else 256 fungal
# genotype values, and everything below, get silently dropped)
ISA_ALIAS={'fungal genotype':'fungal_genotype','state':'state',
           'repository id':'repository_id','fingerprint clade':'fingerprint_clade'}
# taxon-dependent targets: (fungidb_target, other_target)
TAXON={'pathogen_organism':('fungus organism','parasite organism'),
       'pathogen_strain'  :('fungal strain','parasite strain'),
       'pathogen_genotype':('fungal genotype','parasite genotype')}

# samplesheet header -> (kind, target)
#   kind: map|split|taxon|mint|id|drop
SS={}
def _set(hs,kind,target):
    for h in hs: SS[h]=(kind,target)
_set(['geo_loc_name_country','geographic_location_(country_and/or_sea)'],'map','country')
_set(['geo_loc_name_country_continent'],'map','continent')
_set(['geographic_location_(region_and_locality)'],'map','geographic_location')
# country from splitting 'Country: City' is only used when no explicit country
# column exists in the same file, else the two disagree on spelling
_set(['geo_loc_name','Collection Location'],'split','country?|geographic_location')
_set(['lat_lon'],'split','latitude coordinate measurement datum|longitude coordinate measurement datum')
_set(['collection site'],'mint','collection_site')
_set(['Organism','Organism Name','organism_name','Scientific_Name'],'taxon','pathogen_organism')
_set(['strain','Strain','isolate'],'taxon','pathogen_strain')
_set(['HOST','Host','host_scientific_name'],'map','organism')
_set(['host_common_name'],'map','host_common_name')
# 'common_name' in ENA sheets is the PATHOGEN's common name, not the host's:
# constant 'rice blast' across all 19 PRJEB41186 rows whose hosts are variously
# Oryza / Zea mays / Echinochloa / Lolium / Setaria. Redundant with
# fungus_organism / parasite_organism.
_set(['common_name'],'drop','')
_set(['host_sex'],'map','sex')
_set(['host_variety'],'map','host variety')
_set(['host_health_state'],'map','health_status')
_set(['Environmental Material','env_medium','sample_type'],'map','specimen')
_set(['isolation_source','isolation_source_host_associated'],'mint','isolation_source')
_set(['isol_growth_condt'],'map','culture medium')
_set(['subspecf_gen_lin','ecotype'],'map','Lineage')
_set(['Collection_Date','Collection Date','collection date'],'map','Collection date')
_set(['time interval'],'map','Collection Year')
_set(['collected_by','collector_name'],'map','specimen collector')
_set(['collecting_institution'],'mint','collecting_institution')
_set(['genotype'],'taxon','pathogen_genotype')
_set(['ploidy'],'mint','ploidy')
_set(['dev_stage'],'mint','developmental_stage')
_set(['known_pathogenicity'],'mint','known_pathogenicity')
_set(['diagnostic_method'],'mint','diagnostic_method')
_set(['collection_method'],'mint','sample_collection_method')
_set(['samp_collect_device'],'mint','sample_collection_device')
_set(['clearance half-life'],'mint','parasite_clearance_half_life')
# also alignment-derived, and from the ORIGINAL study's alignment at that
SS['Percentage of genome with more or equal to 5x read coverage (%)']=('drop','')
# Assay/alignment metrics are deliberately NOT taken from SRA: the alignments are
# being redone, so these come from the workflow's own *_alignment_stats.tsv
# (sample, raw_fastq_reads, trimmed_total_sequences, reads_mapped,
# trimmed_mapped_pct, raw_mapped_pct, average_read_length, mean_coverage), which
# Nextflow loads as a second entity later. AvgSpotLen in particular would
# directly contradict the recomputed average_read_length.
for _h in ['Assay Type','LibraryLayout','LibrarySelection','LibrarySource',
           'Platform','Instrument','AvgSpotLen']:
    SS[_h]=('drop','')
_set(['Sample Name','Sample_name','Sample ID','Specimen ID','specimen_id','Submitter_Id','Library Name','external_id'],'id','')
for h in ['Run','BioProject','BioSample','SRA Study','Experiment','Bytes','Bases','Consent','DATASTORE filetype',
 'DATASTORE provider','DATASTORE region','create_date','ReleaseDate','version','ENA-FIRST-PUBLIC (run)',
 'ENA-LAST-UPDATE (run)','ENA-LAST-UPDATE','ENA_first_public','ena_first_public','ena_last_update',
 'INSDC_center_name','INSDC_first_public','INSDC_last_update','INSDC_status','INSDC_center_alias','Center Name',
 'ncbi_bioproject_id','ncbi_biosample_id','Name - formula','Name - run','BioSampleModel','Genus','Species',
 'purpose_of_sampling','source_type','env_broad_scale','env_local_scale','Cultivar','alias_tax_id']:
    SS[h]=('drop','')

# NCBI BioSample attributes: harmonized_name where NCBI assigned one, else the
# submitter's attribute_name
_set(['collection_date'],'map','Collection date')
_set(['host'],'map','organism')
_set(['scientific_name'],'taxon','pathogen_organism')
_set(['geographic location (region and locality)'],'map','geographic_location')
_set(['collector name'],'map','specimen collector')
_set(['collecting institution'],'mint','collecting_institution')
_set(['isolation source host associated'],'mint','isolation_source')
_set(['diagnostic method'],'mint','diagnostic_method')
_set(['host_disease'],'map','health_status')
_set(['host_disease_stage'],'mint','host_disease_stage')
_set(['pulmonary_disord'],'mint','pulmonary_disorder')
_set(['biomaterial_provider'],'mint','biomaterial_provider')
_set(['serotype'],'mint','serotype')
_set(['pathogenicity'],'mint','known_pathogenicity')
_set(['common name'],'drop','')
_set(['sample_name','Submitter Id','External Id'],'id','')
for h in ['ENA-CHECKLIST','ENA-FIRST-PUBLIC','ENA-LAST-UPDATE','ENA first public','ENA last update',
 'INSDC center name','INSDC center alias','INSDC first public','INSDC last update','INSDC status']:
    SS[h]=('drop','')

# ================================================================ geography
# Goal: city / state / country / continent mean the same thing everywhere.
#
# 'Characteristics [GeographicLocation]' is used inconsistently across ISA
# studies -- country in 15 of them, but continent in EUSTDY0099 ('Europe') and
# state in EUSTDY0140 ('New Hampshire'). There is no way to tell from the header,
# so the role is pinned per study. Default country; exceptions listed.
GEOLOC_ROLE=collections.defaultdict(lambda:'country',{
    'EUSTDY0099':'continent',   # values are 'Europe'
    'EUSTDY0140':'state',       # values are 'New Hampshire'
})
# Whitespace-variant headers that are genuinely different variables. The ISA
# files carry both 'Characteristics [x]' and 'Characteristics[x]'; normalizing
# whitespace merges two distinct facts, so pin the no-space variant per study.
# case-sensitive per-study header pins (ISA files with two health columns)
ISA_HEADER_PIN={
    ('EUSTDY0137','Characteristics [health_status]'):'infection',   # values 'HIV+'
}
NOSPACE_ROLE={
    ('EUSTDY0161','city'):'state',             # 'Maryland' beside city 'Bethesda'
    ('EUSTDY0103','specimen'):'culture_condition',  # 'Axenic' beside 'Stool sample'
    ('EUSTDY0064','city'):'state',             # 'England'/'Wales' beside country UK
    ('EUSTDY0065','city'):'state',
    ('EUSTDY0138','city'):'state',             # 'Georgia','Pennsylvania',...
}
US_STATES={'alabama':'AL','alaska':'AK','arizona':'AZ','arkansas':'AR','california':'CA',
 'colorado':'CO','connecticut':'CT','delaware':'DE','florida':'FL','georgia':'GA','hawaii':'HI',
 'idaho':'ID','illinois':'IL','indiana':'IN','iowa':'IA','kansas':'KS','kentucky':'KY',
 'louisiana':'LA','maine':'ME','maryland':'MD','massachusetts':'MA','michigan':'MI',
 'minnesota':'MN','mississippi':'MS','missouri':'MO','montana':'MT','nebraska':'NE',
 'nevada':'NV','new hampshire':'NH','new jersey':'NJ','new mexico':'NM','new york':'NY',
 'north carolina':'NC','north dakota':'ND','ohio':'OH','oklahoma':'OK','oregon':'OR',
 'pennsylvania':'PA','rhode island':'RI','south carolina':'SC','south dakota':'SD',
 'tennessee':'TN','texas':'TX','utah':'UT','vermont':'VT','virginia':'VA','washington':'WA',
 'west virginia':'WV','wisconsin':'WI','wyoming':'WY'}
US_ABBREV=set(US_STATES.values())
# Sub-national regions observed in ISA/samplesheet city columns. Explicit list,
# not inference -- an unlisted value stays in city and is reported for curation.
SUBNATIONAL={x.lower() for x in list(US_STATES)+[
 'England','Wales','Scotland','Northern Ireland',
 'Henan','Fujian','Fujian province','Yunnan','Zhejiang','Taiwan','Guangdong','Sichuan',
 'Saxony-Anhalt','Thuringia','Saxony','Bavaria','Lower Saxony',
 'Golestan Province','Golestan','Mazandaran','Gilan',
 'Zurich Kanton','Sardinia','Sicily','Bihar','Odisha','Punjab','Kerala',
 'El Beheira','Kafr El Sheikh','Acre State','Michoacan','Sonora','Coahuila','Nuevo Leon',
 'Corrientes','Occitanie','Vercelli','Veneria','Sikasso','Mpika','Maryland',
 # Canadian provinces/territories -- 'Manitoba' was sitting in city for 97 samples
 'Alberta','British Columbia','Manitoba','New Brunswick','Newfoundland and Labrador',
 'Nova Scotia','Ontario','Prince Edward Island','Quebec','Saskatchewan',
 'Northwest Territories','Nunavut','Yukon',
]}
unresolved_geo=collections.Counter()
uncategorised=set()
collision_eg={}
renamed=[]

def normalize_city(v,state_already=False):
    """Return (geographic_location,state) from a free-text place value.

    Only splits on rules that are checkable: a trailing US state name/abbrev, or
    a leading listed sub-national region. Anything else stays whole in
    geographic_location -- which is why that variable is named generically.
    """
    v=v.strip().rstrip(',').strip()
    if not v: return '',''
    if v.lower() in SUBNATIONAL and not state_already:
        return '',v                                   # value IS a state
    if ',' in v:
        parts=[p.strip() for p in v.split(',') if p.strip()]
        last=parts[-1]
        if last in US_ABBREV or last.lower() in US_STATES:
            return ", ".join(parts[:-1]), last
        if parts[0].lower() in SUBNATIONAL:
            return ", ".join(parts[1:]), parts[0]
        unresolved_geo[v]+=1
        return v,''
    return v,''

# Per-dataset pins. 'isolate' and 'strain' are free-for-all SRA fields that mean
# something different in nearly every submission, so a single global mapping is
# wrong. Keyed on a dataset-name prefix; only these files deviate.
SS_PIN={
 # strain = 1..72 row serial; isolate = RVVC/TVY10 cohort over 72 samples
 ('calbSC5314_Manitoba_2023','strain'):('map','alternative sample ID'),
 ('calbSC5314_Manitoba_2023','isolate'):('map','isolate group'),
 ('cglaCBS138_Manitoba_2023','strain'):('map','alternative sample ID'),
 ('cglaCBS138_Manitoba_2023','isolate'):('map','isolate group'),
 # isolate = 'Sus scrofa', i.e. the host, not the parasite strain
 ('csuiWienI','isolate'):('map','organism'),
 # serotype = '3ADON' / 'NIV', i.e. the Fusarium trichothecene chemotype
 ('fculClass2-1B_Fcul_Miedaner','serotype'):('mint','trichothecene_chemotype'),
}
def ss_rule(exp,h):
    for (pref,hh),rule in SS_PIN.items():
        if hh==h and exp.startswith(pref): return rule
    return SS.get(h)

# 'uncalculated' is an SRA placeholder, not a value -- it was beating the real
# country ("Cote d'Ivoire") into country_2
EMPTY={'','none','na','n/a','n.d.','not applicable','not collected','missing',
       'unspecified','not available','nan','uncalculated','not provided','unknown'}
def clean(v):
    if v is None: return ''
    s=str(v).strip().replace('\\,',',')
    return '' if s.lower() in EMPTY else s

def split_geo(v):
    if ':' in v:
        a,b=v.split(':',1); return a.strip(),b.strip()
    return v.strip(),''
def split_latlon(v):
    m=re.match(r'^\s*([\d.]+)\s*([NS])\s+([\d.]+)\s*([EW])\s*$',v)
    if not m: return '',''
    la=float(m.group(1))*(1 if m.group(2)=='N' else -1)
    lo=float(m.group(3))*(1 if m.group(4)=='E' else -1)
    return str(la),str(lo)



# ================================================================ categories
# Variable hierarchy for the STF/EDA front end. Keyed on the harmonized variable
# name so it applies identically to every organism. Anything unlisted is left
# uncategorised rather than forced into a bucket it does not belong in.
CATEGORIES=[
 ('collection_location','Collection location',[
   'country','country_2','continent','state','geographic_location','collection_site',
   'latitude_coordinate_measurement_datum','longitude_coordinate_measurement_datum']),
 ('collection_event','Collection event',[
   'collection_date','collection_date_original','collection_year','collection_month',
   'specimen_collector','specimen_collector_2','collecting_institution','sample_collection_method',
   'sample_collection_device','sample_collection_time_post_harvest_days','biomaterial_provider']),
 ('host','Host',[
   'organism','host_common_name','host_variety','lab_host','sex','age',
   'health_status','health_status_2','infection','body_temperature','parasite_density_by_microscope',
   'treatment','study_type','host_disease_stage','pulmonary_disorder']),
 ('organism_under_investigation','Organism under investigation',[
   'fungus_organism','parasite_organism','fungal_strain','fungal_strain_2',
   'parasite_strain','parasite_strain_2','fungal_genotype','parasite_genotype',
   'lineage','haplogroup','fingerprint_clade','vni_subdivision','mating_type',
   'ploidy','gp60_genotype','isolate_group','group','parental_and_clone',
   'organism_id','alternative_sample_id','repository_id','cgh_organization',
   'passage_history','serotype','trichothecene_chemotype']),
 ('specimen_and_culture','Specimen and culture',[
   'specimen','specimen_2','isolation_source','developmental_stage','culture_medium','culture_condition',
   'days_in_culture','diagnostic_method','known_pathogenicity']),
 # Reserved for the alignment-stats entity Nextflow adds later. No sample-level
 # variables feed it now -- nothing is taken from SRA for this, by design.
 ('sequencing_assay','Sequencing assay',[
   'raw_fastq_reads','trimmed_total_sequences','reads_mapped','trimmed_mapped_pct',
   'raw_mapped_pct','average_read_length','mean_coverage']),
 ('phenotype_and_drug_response','Phenotype and drug response',[
   'fungal_phenotype','parasite_phenotype','anti_fungal_agent_resistance',
   'resistance_to_itraconazole','phenotype_resistance_to_azole',
   'phenotype_resistance_to_posaconazole','phenotype_resistance_to_voraconazole',
   'parasite_clearance_half_life','triazole','amb','and','csf','fcz','icz','mf','pz','vcz',
   'mic_of_fk506','mic_of_natamycin','mic_of_tebuconazole','mic_of_terbinafine',
   'h2o2','ldopa_ave_std_dev','paraquat_ave_std_dev']),
 ('provenance','Provenance and identity',['dataset_id','sample_display_name']),
]
VAR2CAT={v:(cid,label) for cid,label,vs in CATEGORIES for v in vs}

# ================================================================ dates
# collection_date must be a strict YYYY-MM-DD so the front end can treat it as
# continuous; anything less precise or ambiguous becomes null. Year and month are
# captured separately whenever they are recoverable, and the verbatim string is
# always kept in collection_date_original so nothing is lost.
#
# 28 distinct formats appear in the corpus, including two CONFLICTING slash
# conventions -- '1/16/13' and '11/25/12' are M/D/YY, while '23/3/2009' and
# '19/11/2010' are D/M/YYYY. The convention is therefore inferred per source
# file from that file's own unambiguous rows, never assumed globally.
MONTHS={m.lower():i for i,m in enumerate(
    ['Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec'],1)}
CURYY=26   # 2-digit years above this are last century

def _yy(y):
    y=int(y)
    if y>=100: return y
    return 2000+y if y<=CURYY else 1900+y

def parse_date(v,slash=None):
    """-> (year,month,day,ambiguous) with None for missing parts."""
    v=v.strip()
    if not v: return (None,None,None,False)
    if re.fullmatch(r'\d{4}',v): return (int(v),None,None,False)
    if re.fullmatch(r'\d{4}-\d{4}',v): return (None,None,None,True)   # year range
    m=re.fullmatch(r'(\d{4})-(\d{1,2})',v)
    if m: return (int(m.group(1)),int(m.group(2)),None,False)
    m=re.fullmatch(r'(\d{4})-(\d{1,2})-(\d{1,2})(?:[ T]\d{2}:\d{2}(?::\d{2})?)?',v)
    if m:
        y,mo,d=int(m.group(1)),int(m.group(2)),int(m.group(3))
        # Excel renders a bare YEAR in a date-formatted cell as a date near 1905.
        # All 11 such values in this corpus decode to serials 1983-2009, i.e.
        # they are collection years, not 1905 collections. Recover the year.
        if y<1915:
            try:
                import datetime
                serial=(datetime.date(y,mo,d)-datetime.date(1899,12,30)).days
                if 1900<=serial<=2030: return (serial,None,None,True)
            except ValueError: pass
        return (y,mo,d,False)
    if re.fullmatch(r'\d{8}',v):
        return (int(v[:4]),int(v[4:6]),int(v[6:]),False)
    m=re.fullmatch(r'(\d{1,2})-([A-Za-z]{3})-(\d{2,4})',v)
    if m and m.group(2).lower() in MONTHS:
        return (_yy(m.group(3)),MONTHS[m.group(2).lower()],int(m.group(1)),False)
    m=re.fullmatch(r'(\d{1,2})-([A-Za-z]{3})',v)          # Excel ate the year
    if m and m.group(2).lower() in MONTHS:
        return (None,MONTHS[m.group(2).lower()],None,True)
    m=re.fullmatch(r'(\d{1,2})/(\d{1,2})/(\d{2,4})',v)
    if m:
        a,b,y=int(m.group(1)),int(m.group(2)),_yy(m.group(3))
        if a>12 and b<=12: return (y,b,a,False)            # unambiguous D/M
        if b>12 and a<=12: return (y,a,b,False)            # unambiguous M/D
        if slash=='MDY': return (y,a,b,False)
        if slash=='DMY': return (y,b,a,False)
        return (y,None,None,True)                          # year only, rest unsafe
    return (None,None,None,True)

def infer_slash(values):
    """Decide M/D vs D/M for one source file from its unambiguous rows."""
    mdy=dmy=0
    for v in values:
        m=re.fullmatch(r'(\d{1,2})/(\d{1,2})/(\d{2,4})',v.strip())
        if not m: continue
        a,b=int(m.group(1)),int(m.group(2))
        if a>12 and b<=12: dmy+=1
        elif b>12 and a<=12: mdy+=1
    if mdy and not dmy: return 'MDY'
    if dmy and not mdy: return 'DMY'
    return None

def fix_dates(rows):
    """Rewrite date fields in place; returns per-outcome counts."""
    stats=collections.Counter()
    bysrc=collections.defaultdict(list)
    for r in rows:
        for k in ('collection_date','collection_date_2','collection_year'):
            if r.get(k): bysrc[r.get('_date_src','?')].append(r[k])
    slash={k:infer_slash(v) for k,v in bysrc.items()}
    for r in rows:
        cands=[r[k] for k in ('collection_date','collection_date_2','collection_year') if r.get(k)]
        for k in ('collection_date','collection_date_2','collection_year'): r.pop(k,None)
        if not cands: continue
        sl=slash.get(r.get('_date_src','?'))
        best=None
        for v in cands:
            y,m,d,amb=parse_date(v,sl)
            prec=(y is not None)+(m is not None)+(d is not None)
            if best is None or prec>best[0]: best=(prec,y,m,d,amb,v)
        prec,y,m,d,amb,raw=best
        r['collection_date_original']=raw
        if y and m and d:
            try:
                import datetime; datetime.date(y,m,d)
                r['collection_date']=f"{y:04d}-{m:02d}-{d:02d}"; stats['full YYYY-MM-DD']+=1
            except ValueError: stats['invalid date -> null']+=1
        else:
            stats['null (incomplete/ambiguous)']+=1
        if y: r['collection_year']=str(y)
        if m: r['collection_month']=str(m)
        if amb: stats['ambiguous source value']+=1
    return stats

# ---------------------------------------------------------------- load sources
def load_isa(rel):
    path=os.path.join(REPO_ROOT,rel.removeprefix('ApiCommonMetadataRepository/'))
    with open(path,encoding='utf-8',errors='replace') as f:
        rows=list(csv.reader(f,delimiter='\t'))
    hdr=[h.strip() for h in rows[0]]
    idx={}
    for r in rows[1:]:
        if r and r[0].strip(): idx[r[0].strip()]=dict(zip(hdr,[c.strip() for c in r]))
    return hdr,idx
def load_ncbi():
    """run -> (BioSample, BioProject, attributes), from fetch_ncbi_biosamples.py's cache."""
    d=f'{ROOT}/ncbi'
    attrs=collections.defaultdict(dict)
    for r in csv.DictReader(open(f'{d}/biosample_attributes.tsv',newline=''),delimiter='\t'):
        attrs[r['biosample']][r['attribute']]=r['value']
    return {r['run']:(r['biosample'],r['bioproject'],attrs[r['biosample']])
            for r in csv.DictReader(open(f'{d}/runs.tsv',newline=''),delimiter='\t')}

# ---------------------------------------------------------------- main
mapping=list(csv.DictReader(open(f'{ROOT}/dnaseq_sample_metadata_mapping.txt'),delimiter='\t'))
isa_cache={}
NCBI=load_ncbi()
# A BioSample shared by differently named samples was registered per project, not
# per sample, so its attributes say nothing about any one sample.
BS_SAMPLES=collections.defaultdict(set)
for r in mapping:
    if r['metaDataSource']=='NCBI' and r['joinKey'].split(',')[0] in NCBI:
        BS_SAMPLES[NCBI[r['joinKey'].split(',')[0]][0]].add(r['workflowSample'])
byorg=collections.defaultdict(list)      # org -> sample dicts
usedvars=collections.defaultdict(set)    # org -> varnames
skipped=collections.Counter()
collisions=collections.Counter()         # (target,headerA,headerB) -> n

def put(out,prov,var,val,header):
    """Assign a harmonized variable without ever silently overwriting.

    Two source headers can normalize onto one ontology term while carrying
    genuinely different facts -- EUSTDY0137 has health_status='HIV+' alongside
    'Health status'='cryptocococcal meningitis'; EUSTDY0099 has
    country='Netherlands' alongside GeographicLocation='Europe'. Collapsing
    those to one variable destroys data, and picking a winner by dict order
    destroys it nondeterministically.

    So: first writer (by source column order) keeps the plain name, subsequent
    conflicting writers get <var>_2, _3, ... Every one is logged to
    stf/collision_report.tsv for a curator to rename properly.
    """
    if var not in out or not out[var] or out[var]==val:
        out[var]=val; prov[var]=header; return
    n=2
    while f'{var}_{n}' in out and out[f'{var}_{n}'] and out[f'{var}_{n}']!=val: n+=1
    alt=f'{var}_{n}'
    collisions[(var,prov.get(var,'?'),header,alt)]+=1
    if (var,header) not in collision_eg:
        collision_eg[(var,header)]=(out.get('dataset_name',''),out[var],val)
    out[alt]=val; prov[alt]=header

for r in mapping:
    exp=r['workflowDataRsrcName']; ws=r['workflowSample']; src=r['metaDataSource']
    proj,org=r['projectName'],r['organismAbbrev']
    fungi = (proj=='FungiDB')
    if r['metaRepoSampleName']=='NO_METADATA_MATCH' or not src:
        skipped['no-metadata']+=1
        byorg[org].append({'sample_id':ws,'dataset_name':exp,'dataset_id':dataset_id(r['datasetPresenter']),
                           'project':proj,'sample_display_name':ws})
        usedvars[org].add('sample_display_name')   # else the column is dropped
        continue
    out={'sample_id':ws,'dataset_name':exp,'dataset_id':dataset_id(r['datasetPresenter']),
         'project':proj,'_date_src':src}; prov={}
    # sample_id must stay exactly as the workflow wrote it (it is the join key to
    # *_alignment_stats.tsv). Where ApiCommonMetadataRepository has a different,
    # curated spelling for the same sample, surface that as the display name.
    mrn=r['metaRepoSampleName']
    # matchType 'normalized' means the two names differ ONLY by homoglyph/case
    # (ISA 'CGI10' vs workflow 'CGl10' -- capital I for lowercase l). There the
    # ISA spelling is the corrupted one, so the workflow name stays. The other
    # non-exact types are genuinely cleaner curated names.
    curated = src!='NCBI' and mrn and mrn!='NO_METADATA_MATCH' \
              and r['matchType']!='normalized'
    out['sample_display_name'] = mrn if (curated and mrn!=ws) else ws
    if curated and mrn!=ws: renamed.append((exp,ws,mrn))
    if src=='NCBI' or r['matchType']=='provider':
        row=None
        if r['matchType']=='provider':
            if src not in isa_cache: isa_cache[src]=load_isa(src)
            row=isa_cache[src][1].get(mrn)
            if row is None: skipped['provider-row-missing']+=1
        else:
            hit=NCBI.get(r['joinKey'].split(',')[0])
            if hit is None: skipped['ncbi-run-missing']+=1
            elif len(BS_SAMPLES[hit[0]])>1: skipped['ncbi-shared-biosample']+=1
            else: row=hit[2]; out['_date_src']=hit[1]
        if row is not None:
            for h,v in row.items():
                v=clean(v)
                if not v: continue
                rule=ss_rule(exp,h)
                if not rule: skipped[f'unclassified:{h}']+=1; continue
                kind,target=rule
                if kind in ('drop','id'): continue
                if kind=='map':
                    # minted targets must not be resolved through the corpus:
                    # normh('geographic_location') collides with the
                    # 'GeographicLocation' spelling, which maps to country
                    t=CORPUS.get(normh(target))
                    tv=target if target in MINTED else varname(t['canonical'] if t else target)
                    if tv in ('city','geographic_location'):
                        c,st=normalize_city(v,bool(out.get('state')))
                        if c: put(out,prov,'geographic_location',c,h)
                        if st: put(out,prov,'state',st,h)
                    else: put(out,prov,tv,v,h)
                elif kind=='mint': put(out,prov,target,v,h)
                elif kind=='taxon':
                    canon=TAXON[target][0 if fungi else 1]; put(out,prov,varname(canon),v,h)
                elif kind=='split':
                    a,b=target.split('|')
                    if a.endswith('?'):
                        a=a[:-1]
                        if out.get(varname(a)): a=None
                    va,vb=(split_latlon(v) if a and 'latitude' in a else split_geo(v))
                    if va and a: put(out,prov,varname(a),va,h)
                    if vb:
                        if varname(b)=='geographic_location':
                            c,st=normalize_city(vb,bool(out.get('state')))
                            if c: put(out,prov,'geographic_location',c,h)
                            if st: put(out,prov,'state',st,h)
                        else: put(out,prov,varname(b),vb,h)
    else:
        if src not in isa_cache: isa_cache[src]=load_isa(src)
        hdr,idx=isa_cache[src]
        row=idx.get(r['metaRepoSampleName'])
        if row is None: skipped['isa-row-missing']+=1
        else:
            def _geo_rank(item):
                hb=re.sub(r'^(Characteristics|Comment)\s*\[(.*)\]$',r'\2',item[0].strip()).lower()
                # explicit state/country/continent first; city last so its
                # inferred state can never overwrite a stated one
                return (0 if hb in ('state','country','continent','geographiclocation') else
                        2 if hb=='city' else 1)
            for h,v in sorted(row.items(),key=_geo_rank):
                if h in ('Name','Description','SourceMTOverride','SampleMTOverride',''): continue
                v=clean(v)
                if not v: continue
                study=src.split('/')[-2]
                hb=re.sub(r'^(Characteristics|Comment)\s*\[(.*)\]$',r'\2',h.strip())
                nospace=bool(re.match(r'^(Characteristics|Comment)\[',h.strip()))
                # 'GeographicLocation' means country/state/continent depending on study
                if hb.lower()=='geographiclocation':
                    role=GEOLOC_ROLE[study]
                    if role in ('city','geographic_location'):
                        c,st=normalize_city(v,bool(out.get('state')))
                        if c: put(out,prov,'geographic_location',c,h)
                        if st: put(out,prov,'state',st,h)
                    else: put(out,prov,role,v,h)
                    continue
                pinh=ISA_HEADER_PIN.get((study,h.strip()))
                if pinh:
                    t2=CORPUS.get(normh(pinh))
                    put(out,prov,varname(t2['canonical'] if t2 else pinh),v,h); continue
                # whitespace-variant header pinned to a different variable
                pin=NOSPACE_ROLE.get((study,hb.lower())) if nospace else None
                if pin:
                    if pin=='state': put(out,prov,'state',v,h)
                    else: put(out,prov,pin,v,h)
                    continue
                if hb.lower()=='city':
                    c,st=normalize_city(v,bool(out.get('state')))
                    if c: put(out,prov,'geographic_location',c,h)
                    if st: put(out,prov,'state',st,h)
                    continue
                t=CORPUS.get(normh(h))
                if t: put(out,prov,varname(t['canonical']),v,h); continue
                bare=re.sub(r'^(characteristics|comment)\s*\[(.*)\]$',r'\2',h.strip().lower()).strip()
                if bare in ISA_ALIAS: put(out,prov,ISA_ALIAS[bare],v,h)
                else: skipped[f'isa-unmapped:{h}']+=1
    byorg[org].append(out)
    usedvars[org].update(k for k in out if k not in
        ('sample_id','dataset_name','dataset_id','project','_date_src'))

datestats=collections.Counter()
for org in byorg:
    datestats.update(fix_dates(byorg[org]))
    for d in byorg[org]:
        usedvars[org].discard('collection_date_2'); usedvars[org].discard('_date_src')
        for k in ('collection_date','collection_year','collection_month','collection_date_original'):
            if d.get(k): usedvars[org].add(k)

# collapse duplicate rows per (org, sample_id, dataset_name)  [multi-run samples]
conflicts=collections.Counter()
for org,rows in byorg.items():
    merged={}
    for d in rows:
        k=(d['sample_id'],d['dataset_name'])
        if k not in merged: merged[k]=dict(d)
        else:
            for f,v in d.items():
                if f not in merged[k] or not merged[k][f]: merged[k][f]=v
                elif merged[k][f]!=v: conflicts[f]+=1
    byorg[org]=list(merged.values())

# ---------------------------------------------------------------- write
os.makedirs(OUT,exist_ok=True)
def display_and_def(v):
    if v in MINTED: return MINTED[v]
    for k,t in CORPUS.items():
        if varname(t['canonical'])==v: return (t['canonical'],f"{t['canonical']} ({t['source_id']})")
    return (v.replace('_',' '),'')
def source_id(v):
    if v in MINTED: return ''
    for k,t in CORPUS.items():
        if varname(t['canonical'])==v: return t['source_id']
    return ''

targets=sys.argv[1:] or sorted(byorg)
summary=[]
for org in targets:
    rows=byorg[org]; proj=rows[0]['project']
    vars_=[v for v in sorted(usedvars[org])]
    cols=['sample_id','dataset_id','sample_display_name']+[v for v in vars_ if v!='sample_display_name']
    d=f'{OUT}/{proj}/{org}'; os.makedirs(d,exist_ok=True)
    with open(f'{d}/samples.tsv','w',newline='') as f:
        w=csv.DictWriter(f,fieldnames=cols,delimiter='\t',lineterminator='\n',extrasaction='ignore')
        w.writeheader()
        for x in sorted(rows,key=lambda z:(z['dataset_name'],z['sample_id'])): w.writerow(x)
    with open(f'{d}/header_ontology_mapping.txt','w',newline='') as f:
        w=csv.writer(f,delimiter='\t',lineterminator='\n')
        w.writerow(['headerName','DisplayName','Definition','SourceId','Minted','Category','CategoryDisplayName'])
        w.writerow(['sample_id','Sample ID','Sample identifier as processed by the dnaseq workflow','OBI_0001616','no','',''])
        w.writerow(['dataset_id']+list(MINTED['dataset_id'])+['','yes']+list(VAR2CAT['dataset_id']))
        for v in vars_:
            dn,df=display_and_def(v)
            cid,clab=VAR2CAT.get(v,('',''))
            w.writerow([v,dn,df,source_id(v),'yes' if v in MINTED else 'no',cid,clab])
        uncat=[v for v in vars_ if v not in VAR2CAT]
        if uncat: uncategorised.update(uncat)
    summary.append((org,proj,len(rows),len({x['dataset_name'] for x in rows}),len(vars_)))

with open(f'{ROOT}/stf/dataset_ids.tsv','w',newline='') as f:
    w=csv.writer(f,delimiter='\t',lineterminator='\n')
    w.writerow(['dataset_id','datasetName','project','orgAbbrev','nSamples'])
    seen={}
    for org,rows in byorg.items():
        for d in rows:
            k=d['dataset_name']
            seen.setdefault(k,[d['dataset_id'],k,d['project'],org,0])
            seen[k][4]+=1
    for k in sorted(seen): w.writerow(seen[k])
print(f"dataset_id lookup -> stf/dataset_ids.tsv ({len(seen)} datasets)")
print("date normalization:", dict(datestats))
print("variables with no category:", sorted(uncategorised) or "none")
print(f"sample_display_name differs from sample_id for {len(renamed)} samples")
for e,ws,mrn in sorted(renamed): print(f"   {ws!r:<32} -> {mrn!r:<26} ({e.split('_dnaseq')[0]})")
print(f"{'organism':<18} {'project':<12} {'samples':>7} {'datasets':>8} {'vars':>5}")
for o,p,n,nd,nv in summary: print(f"{o:<18} {p:<12} {n:>7} {nd:>8} {nv:>5}")
print("\nskipped/notes:")
for k,v in skipped.most_common(): print(f"   {k}: {v}")
if conflicts: print("value conflicts when collapsing multi-run samples:",dict(conflicts.most_common(8)))
if collisions:
    with open(f'{ROOT}/stf/collision_report.tsv','w',newline='') as f:
        w=csv.writer(f,delimiter='\t',lineterminator='\n')
        w.writerow(['baseVariable','firstHeader','conflictingHeader','assignedVariable','nSamples',
                    'exampleDataset','exampleFirstValue','exampleConflictingValue','scope','note'])
        for (var,a,b,alt),n in collisions.most_common():
            ds,v1,v2=collision_eg.get((var,b),('','',''))
            resolved = var.startswith('collection_date')
            w.writerow([var,a,b,alt,n,ds,v1,v2,
                        'same source file, same row (two columns describing one sample)',
                        'RESOLVED downstream: consumed by date normalization into '
                        'collection_date/_year/_month' if resolved else
                        'curator: rename assignedVariable to its real meaning'])
    print(f"\nHEADER COLLISIONS -> stf/collision_report.tsv ({len(collisions)} distinct, "
          f"{sum(collisions.values())} sample-values preserved under _N names):")
    for (var,a,b,alt),n in collisions.most_common(14): print(f"   {alt:<26} <- {b!r} (vs {a!r})  x{n}")
else: print("\nno header collisions")
if unresolved_geo:
    with open(f'{ROOT}/stf/unresolved_geography.tsv','w',newline='') as f:
        w=csv.writer(f,delimiter='\t',lineterminator='\n')
        w.writerow(['geographicLocationValue','nSamples','note'])
        for v,n in unresolved_geo.most_common(): w.writerow([v,n,'multi-part value: a state may still be extractable'])
    print(f"\nmulti-part geographic_location values (state not extracted): {len(unresolved_geo)} distinct,"
          f" {sum(unresolved_geo.values())} samples -> stf/unresolved_geography.tsv")
