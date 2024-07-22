import { ComponentFixture, TestBed } from '@angular/core/testing';

import { IcesvgComponent } from './icesvg.component';

describe('IcesvgComponent', () => {
  let component: IcesvgComponent;
  let fixture: ComponentFixture<IcesvgComponent>;

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      declarations: [IcesvgComponent],
    }).compileComponents();
  });

  beforeEach(() => {
    fixture = TestBed.createComponent(IcesvgComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
