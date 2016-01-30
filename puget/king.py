"""
Functions specific to data from King County HMIS extraction.

King County data is provided in the following format:

    data/king/2011_CSV_4_6-1-15
             /2012_CSV_4_6-1-15
             /2013_CSV_4_6-2-15
             /2014_CSV_4_6-1-15/Affiliation.csv
                                Client.csv
                                Disabilities.csv
                                EmploymentEducation.csv
                                Enrollment_families.csv
                                Enrollment.csv
                                EnrollmentCoC.csv
                                Exit.csv
                                Export.csv
                                Funder.csv
                                HealthAndDV.csv
                                IncomeBenefits.csv
                                Inventory.csv
                                Organization.csv
                                Project.csv
                                ProjectCoC.csv
                                Services.csv
                                Site.csv
"""

import pandas as pd
import datetime
import os.path as op
import numpy as np
import json
import puget.utils as pu
import warnings

from puget.data import DATA_PATH
KING_DATA = op.join(DATA_PATH, 'king')

#  Paths of csvs
FILEPATHS = {2011: '2011_CSV_4_6-1-15', 2012: '2012_CSV_4_6-1-15',
             2013: '2013_CSV_4_6-2-15', 2014: '2014_CSV_4_6-1-15'}

# these values translate to unknown data for various reasons. Treat as NANs
CATEGORICAL_UNKNOWN = [8, 9, 99]

# entry/exit suffixes for columns
ENTRY_EXIT_SUFFIX = ['_entry', '_exit']

def std_path_setup(filename, data_dir=None, paths=FILEPATHS, years=None):
    """
    Setup filenames for read_table assuming standard data directory structure.

    Parameters
    ----------
    filename : string
        This should be the filename of the .csv table

    data_dir : string
        full path to general data folder (usually puget/data)

    paths : list
        list of directories inside data_dir to look for csv files in

    years : list
        list of years to include, default is to include all years

    Returns
    ----------
    dict with key of years, value of filenames for all included years
    """
    if years is None:
        years = paths.keys()
    if isinstance(years, (int, float)):
        years = [years]

    path_list = [paths[y] for y in years]
    file_list = []
    for path in path_list:
        file_list.append(op.join(data_dir, path, filename))

    file_dict = dict(zip(years, file_list))
    return file_dict


def read_table(file_dict, data_dir=None, paths=FILEPATHS, years=None,
               columns_to_drop=None, categorical_var=None,
               categorical_unknown=CATEGORICAL_UNKNOWN,
               time_var=None, duplicate_check_columns=None, dedup=True):
    """
    Read in any .csv table from multiple years in the King data.

    Parameters
    ----------
    file_dict : dict or string
        if a dict, keys should be years, values should be full path to files
        if a string, should be the filename of the .csv table and data_dir,
            paths and years parameters are required

    data_dir : string
        full path to general data folder (usually puget/data);
            not required if file_dict is a dictionary

    paths : list
        list of directories inside data_dir to look for csv files in;
            not required if file_dict is a dictionary

    years : list
        list of years to include, default is to include all years;
            not required if file_dict is a dictionary

    columns_to_drop : list
        A list of of columns to drop. The default is None.

    categorical_var : list
        A list of categorical (including binary) variables where values
        listed in categorical_unknown should be recorded as NaNs

    categorical_unknown: list
        values that should be recorded as NaNs for categorical variables
        typically: 8, 9, 99

    time_var : list
        A list of time (variables) in yyyy-mm-dd format that are
        reformatted into pandas timestamps. Default is None.

    duplicate_check_columns : list
        list of columns to conside in deduplication.
          Generally, duplicate rows may happen when the same record is
          registered across the .csv files for each year.

    dedup: boolean
        flag to turn on/off deduplication. Defaults to True

    Returns
    ----------
    dataframe of a csv tables from all included years
    """
    if columns_to_drop is None:
        columns_to_drop = []
    if categorical_var is None:
        categorical_var = []
    if time_var is None:
        time_var = []

    if not isinstance(file_dict, dict):
        if data_dir is None:
            raise ValueError(
                'If file_dict is a string, data_dir must be passed')
        file_dict = std_path_setup(file_dict, data_dir=data_dir, paths=paths)

    file_dict_use = file_dict.copy()

    # Start by reading the first file into a DataFrame
    year, fname = file_dict_use.popitem()
    df = pd.read_csv(fname, low_memory=False)
    df['years'] = year

    # Then, for the rest of the files, append to the DataFrame.
    for year, fname in file_dict_use.items():
        this_df = pd.read_csv(fname, low_memory=False)
        this_df['years'] = year
        df = df.append(this_df)

    # Drop unnecessary columns
    df = df.drop(columns_to_drop, axis=1)

    if dedup:
        if duplicate_check_columns is None:
            warnings.warn('dedup is True but duplicate_check_columns is ' +
                          'None, no deduplication')
        else:
            df = df.drop_duplicates(duplicate_check_columns, keep='last',
                                    inplace=False)

    # Turn values in categorical_unknown in any categorical_var into NaNs
    for col in categorical_var:
        df[col] = df[col].replace(categorical_unknown,
                                  [np.NaN, np.NaN, np.NaN])

    # Reformat yyyy-mm-dd variables to pandas timestamps
    for col in time_var:
        df[col] = pd.to_datetime(df[col], errors='coerce')
    return df


def split_rows_to_columns(df, type_column=None, type_suffix=None,
                          merge_columns=None):
    """
    create separate enty and exit columns for dataframes that have that
    information provided as a column giving the collection stage
    (coded as numerical values) and other columns containing the measurements
    at entry/exit

    Parameters
    ----------
    df: dataframe
        input dataframe

    type_column : string
        name of column containing the types to be remapped to columns

    type_suffix : dict
        keys are values in type_column, values are suffixes to attach to the
        column for that type

    merge_columns: list or string
        name(s) of column(s) containing to merge on.

    Returns
    ----------
    new dataframe with response columns split into *_entry and *_exit columns
    """
    columns_to_rename = list(df.columns.values)
    if isinstance(merge_columns, list):
        for col in merge_columns:
            columns_to_rename.remove(col)
    else:
        columns_to_rename.remove(merge_columns)

    columns_to_rename.remove(type_column)

    # group by each type in turn
    gb = df.groupby(type_column)
    for index, tpl in enumerate(gb):
        name, group = tpl
        rename_dict = dict(zip(
            columns_to_rename,
            [s + type_suffix[name] for s in columns_to_rename]))
        this_df = group.rename(columns=rename_dict).drop(type_column, axis=1)
        if index == 0:
            df_wide = this_df
        else:
            df_wide = pd.merge(df_wide, this_df, how='outer', on=merge_columns)

    return df_wide


def read_entry_exit_table(file_dict=None, data_dir=None, paths=FILEPATHS,
                          years=None, metadata=None,
                          suffixes=ENTRY_EXIT_SUFFIX):
    """
    Read in tables with entry & exit values, convert entry & exit rows to
    columns

    Parameters
    ----------
    file_dict : dict or string
        if a dict, keys should be years, values should be full path to files
        if a string, should be the filename of the .csv table and data_dir,
            paths and years parameters are required

    data_dir : string
        full path to general data folder (usually puget/data);
            not required if file_dict is a dictionary

    paths : list
        list of directories inside data_dir to look for csv files in;
            not required if file_dict is a dictionary

    years : list
        list of years to include, default is to include all years;
            not required if file_dict is a dictionary

    metadata : string or dict
        if dict: metadata dict
        if string: name of json metadata file
        lists of columns to use for deduplication, columns to drop,
        categorical and time-like columns
        ALSO names of columns containing collection stage, and uniqueIDs
             and values indicating entry and exit collection stage

    Returns
    ----------
    dataframe with one row per person per enrollment -- rows containing
    separate entry & exit values are combined with different columns for
    entry & exit
    """
    if not isinstance(metadata, dict):
        metadata = get_metadata_dict(metadata)
    extra_metadata = {'collection_stage_column': None,
                      'entry_stage_val': None,
                      'exit_stage_val': None,
                      'uniqueID': None}
    for k in extra_metadata:
        if k in metadata:
            extra_metadata[k] = metadata.pop(k)
        else:
            raise ValueError(k + ' entry must be present in metadata file')

    df = read_table(file_dict, data_dir=data_dir, paths=paths,
                    years=years, **metadata)

    df_wide = split_rows_to_columns(
            df, type_column=extra_metadata['collection_stage_column'],
            type_suffix=dict(zip([extra_metadata['entry_stage_val'],
                                  extra_metadata['exit_stage_val']],
                                 suffixes)),
            merge_columns=extra_metadata['uniqueID'])

    return df_wide


def get_metadata_dict(metadata_file):
    """Little function to read a JSON metadata file into a dict."""
    metadata_handle = open(metadata_file)
    metadata = json.loads(metadata_handle.read())
    _ = metadata.pop('name')
    return metadata


def get_enrollment(groups=True, file_dict='Enrollment.csv',
                   data_dir=KING_DATA, paths=FILEPATHS, years=None,
                   metadata_file=op.join(DATA_PATH, 'metadata',
                                         'king_enrollment.json'),
                   groupID_column='HouseholdID'):
    """
    Read in the Enrollment tables from King.

    Return rows with some minor clean-up that
    includes dropping unusable columns de-deplication.

    Parameters
    ----------
    groups : boolean
        If true, only return rows for groups (>1 person)

    file_dict : dict or string
        if a dict, keys should be years, values should be full path to files
        if a string, should be the filename of the .csv table and data_dir,
            paths and years parameters are required

    data_dir : string
        full path to general data folder (usually puget/data);
            not required if file_dict is a dictionary

    paths : list
        list of directories inside data_dir to look for csv files in;
            not required if file_dict is a dictionary

    years : list
        list of years to include, default is to include all years;
            not required if file_dict is a dictionary

    metadata_file : string
        name of json metadata file with lists of columns to use for
        deduplication, columns to drop, categorical and time-like columns

    groupID_column : string
        column to use for identifying groups within the
        enrollments (ie households)

    Returns
    ----------
    dataframe with rows representing enrollment record of a person per
        enrollment, optionally with people who are not in groups removed
    """
    metadata = get_metadata_dict(metadata_file)
    df = read_table(file_dict, data_dir=data_dir, paths=paths,
                    years=years, **metadata)
    # Now, group by HouseholdID, and only keep the groups where there are
    # more than one ProjectEntryID.
    # The new dataframe should represent families
    # (as opposed to single people).

    if groups:
        gb = df.groupby(groupID_column)

        def more_than_one(x): return (x.shape[0] > 1)
        df = gb.filter(more_than_one)

    df = df.sort_values(by=groupID_column)

    return df


def get_exit(file_dict='Exit.csv',
             data_dir=KING_DATA, paths=FILEPATHS, years=None,
             metadata_file=op.join(DATA_PATH, 'metadata', 'king_exit.json'),
             df_destination_column='Destination'):
    """
    Read in the Exit tables from King and map Destinations.

    Parameters
    ----------
    file_dict : dict or string
        if a dict, keys should be years, values should be full path to files
        if a string, should be the filename of the .csv table and data_dir,
            paths and years parameters are required

    data_dir : string
        full path to general data folder (usually puget/data);
            not required if file_dict is a dictionary

    paths : list
        list of directories inside data_dir to look for csv files in;
            not required if file_dict is a dictionary

    years : list
        list of years to include, default is to include all years;
            not required if file_dict is a dictionary

    metadata_file : string
        name of json metadata file with lists of columns to use for
        deduplication, columns to drop, categorical and time-like columns

    df_destination_column : string
        column containing the numeric destination codes

    Returns
    ----------
    dataframe with rows representing exit record of a person per enrollment
    """
    metadata = get_metadata_dict(metadata_file)
    df = read_table(file_dict, data_dir=data_dir, paths=paths,
                    years=years, **metadata)

    df_merge = pu.merge_destination(
        df, df_destination_column=df_destination_column)

    return df_merge


def get_client(file_dict='Client.csv',
               data_dir=KING_DATA, paths=FILEPATHS, years=None,
               metadata_file=None, dob_column='DOB'):
    """
    Read in the Client tables from King and map Destinations.

    Parameters
    ----------
    file_dict : dict or string
        if a dict, keys should be years, values should be full path to files
        if a string, should be the filename of the .csv table and data_dir,
            paths and years parameters are required

    data_dir : string
        full path to general data folder (usually puget/data);
            not required if file_dict is a dictionary

    paths : list
        list of directories inside data_dir to look for csv files in;
            not required if file_dict is a dictionary

    years : list
        list of years to include, default is to include all years;
            not required if file_dict is a dictionary

    metadata_file : string
        name of json metadata file with lists of columns to use for
        deduplication, columns to drop, categorical and time-like columns,
        ALSO lists of boolean and numerically coded columns and
             personalID column

    dob_column: string
        name of column containing the client date of birth

    Returns
    ----------
    dataframe with rows representing demographic information of a person
    """
    metadata = get_metadata_dict(metadata_file)

    # Don't want to deduplicate before checking if DOB is sane because the last
    # entry is taken in deduplication but the first entry indicates how early
    # they entered the system
    duplicate_check_columns = metadata.pop('duplicate_check_columns')
    if 'boolean' in metadata:
        boolean_cols = metadata.pop('boolean')
    else:
        boolean_cols = []
        warnings.warn('boolean_cols is None')
    if 'numeric_code' in metadata:
        numeric_cols = metadata.pop('numeric_code')
    else:
        numeric_cols = []
        warnings.warn('numeric_cols is None')
    if 'pid_column' in metadata:
        pid_column = metadata.pop('pid_column')
    else:
        raise ValueError('pid_column entry must be present in metadata file')

    df = read_table(file_dict, data_dir=data_dir, paths=paths,
                    years=years, **metadata, dedup=False)
    df = df.set_index(np.arange(df.shape[0]))

    bad_dob = np.logical_or(df[dob_column] >
                            pd.to_datetime(df.years.astype(str) +
                                           "/12/31", format='%Y/%m/%d'),
                            df[dob_column] < pd.to_datetime(
                                '1900/1/1', format='%Y/%m/%d'))
    n_bad_dob = np.sum(bad_dob)

    # set any bad DOBs to NaNs. Also set to NaN if the same DOB looks bad in
    # one year but ok in the other -- ie if the DOB was in the future when it
    # was first entered it but in the past later
    gb = df.groupby(pid_column)
    for pid, group in gb:
        if np.sum(bad_dob[group.index]) > 0:
            n_entries = group.shape[0]
            if n_entries == 1:
                df.loc[group.index, dob_column] = pd.NaT
            else:
                if max(group[dob_column]) == min(group[dob_column]):
                    df.loc[group.index, dob_column] = pd.NaT
                else:
                    df.loc[group.index[np.where(bad_dob[group.index] == True)],
                           dob_column] = pd.NaT

    print('Found %d entries with bad DOBs' % n_bad_dob)

    # drop years column -- this is the year associated with the csv file
    df = df.drop('years', axis=1)
    # perform partial deduplication that was skipped in read_table,
    #  but don't deduplicate time_var, boolean or numeric columns until after
    #  resolving differences
    mid_dedup_cols = list(set(list(duplicate_check_columns) +
                              list(metadata['time_var']) +
                              list(boolean_cols) + list(numeric_cols) +
                              list(pid_column)))
    df = df.drop_duplicates(mid_dedup_cols, keep='last', inplace=False)

    # iterate through people with more than one entry and resolve differences.
    # Set all rows to the same sensible value
    gb = df.groupby(pid_column)
    n_entries = gb.size()
    for pid, group in gb:
        # turn off SettingWithCopy warning for this object
        group.is_copy = False

        if n_entries.loc[pid] > 1:
            # for differences in time columns, if the difference is less than
            # a year then take the midpoint, otherwise set to NaN
            for col in metadata['time_var']:
                if len(np.unique(group[col])) > 1:
                    is_valid = ~pd.isnull(group[col])
                    n_valid = np.sum(is_valid)
                    if n_valid == 1:
                        group[col] = group[col][is_valid].values[0]
                    elif n_valid > 1:
                        t_diff = np.max(group[col]) - np.min(group[col])
                        if t_diff < datetime.timedelta(365):
                            t_diff_sec = t_diff.seconds + 86400 * t_diff.days
                            new_date = (np.min(group[col]) +
                                        datetime.timedelta(
                                            seconds=t_diff_sec / 2.)).date()
                            group[col] = pd.datetime(new_date.year,
                                                     new_date.month,
                                                     new_date.day)
                        else:
                            group[col] = pd.NaT

            # for differences in boolean columns, if ever true then set to true
            for col in boolean_cols:
                if len(np.unique(group[col])) > 1:
                    is_valid = ~pd.isnull(group[col])
                    n_valid = np.sum(is_valid)
                    if n_valid == 1:
                        group[col] = group[col][is_valid].values[0]
                    elif n_valid > 1:
                        group[col] = np.max(group[col][is_valid])

            # for differences in numeric type columns, if there are conflicting
            # valid answers, set to NaN
            for col in numeric_cols:
                if len(np.unique(group[col])) > 1:
                    is_valid = ~pd.isnull(group[col])
                    n_valid = np.sum(is_valid)
                    if n_valid == 1:
                        group[col] = group[col][is_valid].values[0]
                    elif n_valid > 1:
                        group[col] = np.nan

            # push these changes back into the dataframe
            df.iloc[np.where(df[pid_column] == pid)[0]] = group

    # Now all rows with the same pid_column have identical time_var,
    # boolean & numeric_col values so we can perform full deduplication
    # that was skipped in read_table,
    df = df.drop_duplicates(duplicate_check_columns, keep='last',
                            inplace=False)

    return df


def get_disabilities(file_dict='Disabilities.csv',
                     data_dir=KING_DATA, paths=FILEPATHS, years=None,
                     metadata_file=op.join(DATA_PATH, 'metadata',
                                           'king_disabilities.json'),
                     disability_type_file=op.join(DATA_PATH, 'metadata',
                                                  'disability_type.json')):
    """
    Read in the Disabilities tables from King, convert sets of disablity type
    and response rows to columns to reduce to one row per
    primaryID (ie ProjectEntryID) with a column per disability type

    Parameters
    ----------
    file_dict : dict or string
        if a dict, keys should be years, values should be full path to files
        if a string, should be the filename of the .csv table and data_dir,
            paths and years parameters are required

    data_dir : string
        full path to general data folder (usually puget/data);
            not required if file_dict is a dictionary

    paths : list
        list of directories inside data_dir to look for csv files in;
            not required if file_dict is a dictionary

    years : list
        list of years to include, default is to include all years;
            not required if file_dict is a dictionary

    metadata_file : string
        name of json metadata file with lists of columns to use for
        deduplication, columns to drop, categorical and time-like columns
        ALSO names of columns containing collection stage, type, response and
             uniqueIDs and values indicating entry and exit collection stage

    disability_type_file : string
        name of json file with mapping between disability numeric codes and
        string description

    Returns
    ----------
    dataframe with rows representing presence of disability types at entry &
        exit of a person per enrollment
    """
    metadata = get_metadata_dict(metadata_file)
    extra_metadata = {'type_column': None,
                      'response_column': None}

    for k in extra_metadata:
        if k in metadata:
            extra_metadata[k] = metadata.pop(k)
        else:
            raise ValueError(k + ' entry must be present in metadata file')

    extra_metadata['uniqueID'] = metadata['uniqueID']

    stage_suffixes = ENTRY_EXIT_SUFFIX
    df_stage = read_entry_exit_table(file_dict=file_dict, data_dir=data_dir,
                                     paths=paths, years=years,
                                     metadata=metadata,
                                     suffixes=stage_suffixes)

    mapping_dict = get_metadata_dict(disability_type_file)
    # convert to integer keys
    mapping_dict = {int(k): v for k, v in mapping_dict.items()}

    type_suffixes = ['_' + s for s in mapping_dict.values()]
    merge_columns = [extra_metadata['uniqueID'],
                     extra_metadata['type_column'] + stage_suffixes[1],
                     extra_metadata['response_column'] + stage_suffixes[1]]
    df_type1 = split_rows_to_columns(
                    df_stage, type_column=(extra_metadata['type_column'] +
                                           stage_suffixes[0]),
                    type_suffix=dict(zip(list(mapping_dict.keys()),
                                         type_suffixes)),
                    merge_columns=merge_columns)

    merge_columns = [extra_metadata['uniqueID']]
    for ts in type_suffixes:
        col = extra_metadata['response_column'] + stage_suffixes[0] + ts
        if col in list(df_type1.columns.values):
            merge_columns.append(col)
    df_wide = split_rows_to_columns(
                    df_type1, type_column=(extra_metadata['type_column'] +
                                           stage_suffixes[1]),
                    type_suffix=dict(zip(list(mapping_dict.keys()),
                                         type_suffixes)),
                    merge_columns=merge_columns)

    response_cols = []
    new_cols = []
    for ss in stage_suffixes:
        for i, ts in enumerate(type_suffixes):
            col = extra_metadata['response_column'] + ss + ts
            if col in list(df_wide.columns.values):
                response_cols.append(col)
                new_cols.append(ts[1:] + ss)

    rename_dict = dict(zip(response_cols, new_cols))
    df_wide = df_wide.rename(columns=rename_dict)

    return df_wide
