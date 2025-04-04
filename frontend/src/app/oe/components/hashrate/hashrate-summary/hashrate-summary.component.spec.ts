import { ComponentFixture, TestBed } from '@angular/core/testing';

import { HashrateSummaryComponent } from './hashrate-summary.component';

describe('HashrateSummaryComponent', () => {
  let component: HashrateSummaryComponent;
  let fixture: ComponentFixture<HashrateSummaryComponent>;

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      declarations: [ HashrateSummaryComponent ]
    })
    .compileComponents();
  });

  beforeEach(() => {
    fixture = TestBed.createComponent(HashrateSummaryComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
