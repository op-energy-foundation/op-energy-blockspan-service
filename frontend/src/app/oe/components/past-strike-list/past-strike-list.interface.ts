import { TableColumn } from './../../interfaces/oe-energy.interface';

export const TABLE_HEADERS: TableColumn[] = [
  {
    displayName: 'Sr No',
    columnKey: '',
    isSrNo: true
  },
  { displayName: 'Block', columnKey: 'blockTimeStrikePastBlock' },
  {
    displayName: 'Creation Time',
    columnKey: 'blockTimeStrikePastCreationTime',
  },
  {
    displayName: 'Future Strike Creation Time',
    columnKey: 'blockTimeStrikePastFutureStrikeCreationTime',
  },
  { displayName: 'NLock Time', columnKey: 'blockTimeStrikePastNlocktime' },
  {
    displayName: 'Observed Block Mediantime',
    columnKey: 'blockTimeStrikePastObservedBlockMediantime',
  },
  {
    displayName: 'Observed Result',
    columnKey: 'blockTimeStrikePastObservedResult',
  },
];

export const URL_PAST_STRIKES_NEWEST_TO_OLDEST =
  'past_strikes_newest_to_oldest';

export const MAX_DISPLAYED_STRIKES = 100;
