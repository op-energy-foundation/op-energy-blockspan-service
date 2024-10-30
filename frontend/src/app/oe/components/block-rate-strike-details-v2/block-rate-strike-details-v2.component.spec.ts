import { ComponentFixture, TestBed } from '@angular/core/testing';

import { BlockRateStrikeDetailsV2Component } from './block-rate-strike-details-v2.component';

describe('BlockRateStrikeDetailsV2Component', () => {
  let component: BlockRateStrikeDetailsV2Component;
  let fixture: ComponentFixture<BlockRateStrikeDetailsV2Component>;

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      declarations: [BlockRateStrikeDetailsV2Component],
    }).compileComponents();
  });

  beforeEach(() => {
    fixture = TestBed.createComponent(BlockRateStrikeDetailsV2Component);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
