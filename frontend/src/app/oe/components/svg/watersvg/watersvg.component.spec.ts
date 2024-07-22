import { ComponentFixture, TestBed } from '@angular/core/testing';

import { WatersvgComponent } from './watersvg.component';

describe('WatersvgComponent', () => {
  let component: WatersvgComponent;
  let fixture: ComponentFixture<WatersvgComponent>;

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      declarations: [WatersvgComponent],
    }).compileComponents();
  });

  beforeEach(() => {
    fixture = TestBed.createComponent(WatersvgComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
