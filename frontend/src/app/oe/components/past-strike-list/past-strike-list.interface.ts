import { TableColumn } from './../../interfaces/oe-energy.interface';

export const TABLE_HEADERS: TableColumn[] = [
  {
    displayName: 'Sr No',
    columnKey: '',
    isSrNo: true,
  },
  { displayName: 'Block', columnKey: 'block' },
  // {
  //   displayName: 'Creation Time',
  //   columnKey: 'creationTime',
  // },
  // {
  //   displayName: 'Future Strike Creation Time',
  //   columnKey: 'futureStrikeCreationTime',
  // },
  { displayName: 'NLock Time', columnKey: 'nlocktime' },
  {
    displayName: 'Observed Block Mediantime',
    columnKey: 'observedBlockMediantime',
  },
  {
    displayName: 'Observed Result',
    columnKey: 'observedResult',
  },
];

export const URL_PAST_STRIKES_NEWEST_TO_OLDEST =
  'past_strikes_newest_to_oldest';
