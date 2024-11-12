import { ComponentFixture, TestBed } from '@angular/core/testing';

import { BlockrateStrikeSummaryV2Component } from './blockrate-strike-summary-v2.component';

describe('BlockrateStrikeSummaryV2Component', () => {
  let component: BlockrateStrikeSummaryV2Component;
  let fixture: ComponentFixture<BlockrateStrikeSummaryV2Component>;

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      declarations: [ BlockrateStrikeSummaryV2Component ]
    })
    .compileComponents();
  });

  beforeEach(() => {
    fixture = TestBed.createComponent(BlockrateStrikeSummaryV2Component);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
