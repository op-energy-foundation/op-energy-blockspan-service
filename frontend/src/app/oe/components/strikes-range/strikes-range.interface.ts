import { TableColumn } from '../../interfaces/oe-energy.interface';

export const TABLE_HEADERS: TableColumn[] = [
  {
    displayName: 'Sr No',
    columnKey: '',
    isSrNo: true,
  },
  { displayName: 'Block', columnKey: 'block' },
  { displayName: 'Median Time', columnKey: 'strikeMediantime' },
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
