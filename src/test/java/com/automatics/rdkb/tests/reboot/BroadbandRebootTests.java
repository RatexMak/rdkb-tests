/*
 * Copyright 2021 Comcast Cable Communications Management, LLC
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 * SPDX-License-Identifier: Apache-2.0
 */

package com.automatics.rdkb.tests.reboot;

import org.testng.annotations.Test;

import com.automatics.annotations.TestDetails;
import com.automatics.constants.DataProviderConstants;
import com.automatics.device.Dut;
import com.automatics.enums.ExecutionStatus;
import com.automatics.exceptions.TestException;
import com.automatics.rdkb.TestGroup;
import com.automatics.rdkb.constants.BroadBandCommandConstants;
import com.automatics.rdkb.constants.BroadBandConnectedClientTestConstants;
import com.automatics.rdkb.constants.BroadBandTestConstants;
import com.automatics.rdkb.constants.BroadBandWebPaConstants;
import com.automatics.rdkb.constants.WebPaParamConstants.WebPaDataTypes;
import com.automatics.rdkb.reboot.BootTimeUtils;
import com.automatics.rdkb.utils.BroadBandCommonUtils;
import com.automatics.rdkb.utils.BroadbandPropertyFileHandler;
import com.automatics.rdkb.utils.CommonUtils;
import com.automatics.rdkb.utils.DeviceModeHandler;
import com.automatics.rdkb.utils.snmp.BroadBandSnmpMib;
import com.automatics.rdkb.utils.snmp.BroadBandSnmpUtils;
import com.automatics.rdkb.utils.webpa.BroadBandWebPaUtils;
import com.automatics.rdkb.utils.wifi.BroadBandWiFiUtils;
import com.automatics.snmp.SnmpDataType;
import com.automatics.tap.AutomaticsTapApi;
import com.automatics.test.AutomaticsTestBase;
import com.automatics.utils.CommonMethods;
import com.automatics.utils.xconf.XConfUtils;

public class BroadbandRebootTests extends AutomaticsTestBase {
	
	/**
	 * Test to verify Default values after factoryreset and value persistent after
	 * reboot
	 * <ol>
	 * <li>1. Update the DNS cache size to 15000 using webpa</li>
	 * <li>2. Update auto exclude enable as true and exclude url as mock xconf url
	 * using webpa</li>
	 * <li>3. Set credEnable to true and CredUse to true using webpa/dmcli</li>
	 * <li>4. Enable Wifi Blaster Feature Using Webpa</li>
	 * <li>5. Enable Wifi Blaster trigger paramUsing Webpa</li>
	 * <li>6. Set and Verify Wifi Blaster PacketSize Parameter to 1000</li>
	 * <li>7. Set and Verify Wifi Blaster SampleDuration Parameter to 5000</li>
	 * <li>8. Set and Verify Wifi Blaster NumberOfSamples Parameter to 90</li>
	 * <li>9. Set and Verify Wifi Blaster PlanID Parameter </li>
	 * <li>10. Set and Verify Wifi Blaster stepID Parameter to 30</li>
	 * <li>11. Set and Verify Wifi Blaster stepID SourcMac Parameter to device CM
	 * Mac</li>
	 * <li>12. Set and Verify Wifi Blaster steID DestMac Parameter </li>
	 * <li>13. Update force wifi disable value as true using webpa</li>
	 * <li>14. Reboot the device</li>
	 * <li>15. Verify DNS cache size is persist after reboot using webpa</li>
	 * <li>16. Verify Auto exclude enable status and xconf url using webpa after
	 * reboot</li>
	 * <li>17. Verify cred dwnld enable is true and use is true using webpa</li>
	 * <li>18. Verify Wifi Blaster feature is enabled after reboot</li>
	 * <li>19. Verify Wifi Blaster trigger param is enabled after reboot</li>
	 * <li>20. Verify Wifi Blaster PacketSize parameter is persisting after
	 * reboot</li>
	 * <li>21. Verify Wifi Blaster SampleDuration parameter is persisting after
	 * reboot</li>
	 * <li>22. Verify Wifi Blaster NumberOfSamples parameter is persisting after
	 * reboot</li>
	 * <li>23. Verify Wifi Blaster PlanID parameter is not persisting after
	 * reboot</li>
	 * <li>24. Verify Wifi Blaster StepID parameter is not persisting after
	 * reboot</li>
	 * <li>25. Verify Wifi Blaster SourceMac parameter is not persisting after
	 * reboot</li>
	 * <li>26. Verify Wifi Blaster DestMac parameter is not persisting after
	 * reboot</li>
	 * <li>27. Verify force wifi disable value as true after reboot</li>
	 * <li>28. Verify 2.4Ghz radio has been disabled</li>
	 * <li>29. Verify 5Ghz radio has been disabled</li>
	 * <li>30. Do factory reset the device using webpa</li>
	 * <li>31. Verify rabid dns cache size value set to default value of 10000 after
	 * factory reset</li>
	 * <li>32. Verify Auto exclude enable status and xconf url using webpa after
	 * factory reset</li>
	 * <li>33. Verify cred dwnld enable is false and use is false using
	 * webpa/dmcli</li>
	 * <li>34. Validate Wifi Blaster RFC parameter is disabled by default</li>
	 * <li>35. Validate Wifi Blaster Report Active measurements parameter is
	 * disabled by default</li>
	 * <li>36. Validate Wifi Blaster Packet Size parameter is 1470 by default</li>
	 * <li>37. Validate Wifi Blaster SampleDuration parameter is 400 by default</li>
	 * <li>38. Validate Wifi Blaster NumberOfSamples parameter is 5 by default</li>
	 * <li>39. Validate Wifi Blaster PlanID parameter is Null by default</li>
	 * <li>40. Validate Wifi Blaster StepNumberOfEntries parameter is 32 by
	 * default</li>
	 * <li>41. Validate Wifi Blaster Step.{i}.StepID parameter is 0 by default for
	 * selected values of 1,9,16,31</li>
	 * <li>42. Validate Wifi Blaster Step.{i}.SourceMac parameter is Null by default
	 * for selected values of 1,9,16,31</li>
	 * <li>43. Validate Wifi Blaster Step.{i}.StepID parameter is Null by default
	 * for selected values of 1,9,16,31</li>
	 * <li>44. Verify force wifi disable value set to default after factory
	 * reset</li>
	 * <li>45. Verify default value of selfheal aggressive Interval is 5 min using
	 * webpa</li>
	 * <li>46. Verify default value of non-critical selfheal interval is 15 min
	 * using webpa</li>
	 * <li>47. Verify RDKFirmwareUpgrader tr181 is disabled by default</li>
	 * <li>48. Enable RDKFirmwareUpgrader tr181 using webpa</li>
	 * <li>49. Verify new firmware schedule script is present under /etc</li>
	 * </ol>
	 * 
	 * @param device
	 * 
	 * @author Betel Costrow, RamaTeja Meduri
	 * @refactor Athira
	 */

	@Test(enabled = true, dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, groups = TestGroup.SYSTEM)
	@TestDetails(testUID = "TC-RDKB-FACTORYRESET_REBOOT-1001")
	public void testToVerifyDefaultAndPersistanceValueOverReboot(Dut device) {

		LOGGER.info("#######################################################################################");
		LOGGER.info("STARTING TEST CASE: TC-RDKB-FACTORYRESET_REBOOT-1001");
		LOGGER.info(
				"TEST DESCRIPTION: Test to verify default values after factory reset and value persistance over reboot");
		LOGGER.info("TEST STEPS : ");
		LOGGER.info("1. Update the DNS cache size to 15000 using webpa");
		LOGGER.info("2. Update auto exclude enable as true and exclude url as mock xconf url using webpa");
		LOGGER.info("3. Set credEnable to true and CredUse to true using webpa/dmcli");
		LOGGER.info("4. Enable Wifi Blaster Feature Using Webpa");
		LOGGER.info("5. Enable Wifi Blaster trigger paramUsing Webpa");
		LOGGER.info("6. Set and Verify Wifi Blaster PacketSize Parameter to 1000");
		LOGGER.info("7. Set and Verify Wifi Blaster SampleDuration Parameter to 5000");
		LOGGER.info("8. Set and Verify Wifi Blaster NumberOfSamples Parameter to 90");
		LOGGER.info("9. Set and Verify Wifi Blaster PlanID Parameter ");
		LOGGER.info("10. Set and Verify Wifi Blaster stepID Parameter to 30");
		LOGGER.info("11. Set and Verify Wifi Blaster stepID SourcMac Parameter to device CM Mac");
		LOGGER.info("12. Set and Verify Wifi Blaster steID DestMac Parameter");
		LOGGER.info("13. Update force wifi disable value as true using webpa");
		LOGGER.info("14. Reboot the device");
		LOGGER.info("15. Verify DNS cache size is persist after reboot using webpa");
		LOGGER.info("16. Verify Auto exclude enable status and xconf url using webpa after reboot");
		LOGGER.info("17. Verify cred dwnld enable is true and use is true using webpa");
		LOGGER.info("18. Verify Wifi Balster feature is enabled after reboot");
		LOGGER.info("19. Verify Wifi Balster trigger param is enabled after reboot");
		LOGGER.info("20. Verify Wifi Balster PacketSize parameter is persisting  after reboot");
		LOGGER.info("21. Verify Wifi Balster SampleDuration parameter is persisting  after reboot");
		LOGGER.info("22. Verify Wifi Balster NumberOfSamples parameter is persisting  after reboot");
		LOGGER.info("23. Verify Wifi Balster PlanID parameter is not persisting  after reboot");
		LOGGER.info("24. Verify Wifi Balster StepID parameter is not persisting  after reboot");
		LOGGER.info("25. Verify Wifi Balster SourceMac parameter is not persisting  after reboot");
		LOGGER.info("26. Verify Wifi Balster DestMac parameter is not persisting  after reboot");
		LOGGER.info("27. Verify  force wifi disable value as true after reboot");
		LOGGER.info("28. Verify 2.4Ghz radio has been disabled");
		LOGGER.info("29. Verify 5Ghz radio has been disabled");
		LOGGER.info("30. Do factory reset the device using webpa");
		LOGGER.info("31. Verify rabid dns cache size value set to default value of 10000 after factory reset");
		LOGGER.info("32. Verify Auto exclude enable status and xconf url using webpa after factory reset");
		LOGGER.info("33. Verify cred dwnld enable is false and use is false using webpa/dmcli");
		LOGGER.info("34. Validate Wifi Blaster RFC parameter is disabled by default");
		LOGGER.info("35. Validate Wifi Blaster Report Active measurements parameter is disabled by default");
		LOGGER.info("36. Validate Wifi Blaster Packet Size parameter is 1470 by default");
		LOGGER.info("37. Validate Wifi Blaster SampleDuration parameter is 400 by default");
		LOGGER.info("38. Validate Wifi Blaster NumberOfSamples parameter is 5 by default");
		LOGGER.info("39. Validate Wifi Blaster PlanID parameter is Blank/Empty by default");
		LOGGER.info("40. Validate Wifi Blaster StepNumberOfEntries parameter is 32 by default");
		LOGGER.info(
				"41. Validate Wifi Blaster Step.{i}.StepID parameter is 0 by default for selected values of 1,9,16,31");
		LOGGER.info(
				"42. Validate Wifi Blaster Step.{i}.SourceMac parameter is Blank/Empty by default for selected values of 1,9,16,31");
		LOGGER.info(
				"43. Validate Wifi Blaster Step.{i}.StepID parameter is Blank/Empty by default for  selected values of 1,9,16,31");
		LOGGER.info("44. Verify force wifi disable value set to default after factory reset");
		LOGGER.info("45. Verify default value of selfheal aggressive Interval is 5 min using webpa");
		LOGGER.info("46. Verify default value of non-critical selfheal interval is 15 min using webpa");
		LOGGER.info("47. Verify RDKFirmwareUpgrader tr181 is disabled by default");
		LOGGER.info("48. Enable RDKFirmwareUpgrader tr181 using webpa");
		LOGGER.info("49. Verify new firmware schedule script is present under /etc");
		LOGGER.info("#####################################################################################");
		// Variable Declaration begins
		int stepNumber = BroadBandTestConstants.CONSTANT_1;
		String testCaseId = "TC-RDKB-FACTORYRESET_REBOOT-001";
		String stepNum = "s" + stepNumber;
		String errorMessage = null;
		String response = null;
		boolean status = false;
		boolean firmwareUpgradeEnable = false;
		boolean isBusinessClsDevice = DeviceModeHandler.isBusinessClassDevice(device);
		// Variable Declaration Ends

		try {
			/**
			 * Step 1 : Update the DNS cache size to 15000 using webpa
			 */
			if (!isBusinessClsDevice) {
				LOGGER.info("**********************************************************************************");
				LOGGER.info("STEP " + stepNumber + ": DESCRIPTION : Update the DNS cache size to 15000 using webpa");
				LOGGER.info("STEP " + stepNumber
						+ ": ACTION : Execute webpa set command: parameter: Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.RabidFramework.DNSCacheSize uint 15000");
				LOGGER.info("STEP " + stepNumber + ": EXPECTED : Webpa set operation should be successful");
				LOGGER.info("**********************************************************************************");
				status = BroadBandWebPaUtils.setVerifyWebPAInPolledDuration(device, tapEnv,
						BroadBandWebPaConstants.WEBPA_PARAM_RABID_DNS_CACHE_SIZE, BroadBandTestConstants.CONSTANT_2,
						BroadBandTestConstants.STRING_15000, BroadBandTestConstants.THREE_MINUTE_IN_MILLIS,
						BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
				if (status) {
					LOGGER.info("STEP " + stepNumber
							+ ": ACTUAL : Successfully updated DNS cache size as 15000 using webpa");
				} else {
					LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
				}
				LOGGER.info("**********************************************************************************");
				tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);
			} else {
				stepNum = "S" + stepNumber;
				LOGGER.info("STEP " + stepNumber
						+ " : ACTUAL : Updating the DNS cache size to 15000 is not applicable for business class devices");
				LOGGER.info("**********************************************************************************");
				tapEnv.updateExecutionForAllStatus(device, testCaseId, stepNum, ExecutionStatus.NOT_APPLICABLE,
						BroadBandTestConstants.NA_MSG_FOR_BUSINESS_CLASS_DEVICES, false);
			}

			/**
			 * Step 2 : Update auto exclude enable as true and exclude url as mock xconf url
			 * using webpa
			 */
			stepNum = "s" + (++stepNumber);
			status = false;
			LOGGER.info("******************************************************************************");
			LOGGER.info("STEP " + stepNumber
					+ ": DESCRIPTION: Update auto exclude enable as true and exclude url as mock xconf url using webpa");
			LOGGER.info("STEP " + stepNumber
					+ ": ACTION: Execute webpa command: Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.FWUpdate.AutoExcluded.Enable as true"
					+ "Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.FWUpdate.AutoExcluded.XconfUrl");
			LOGGER.info("STEP " + stepNumber
					+ ": EXPECTED: Webpa set operation should success and auto excluded should be enabled and auto exclude url should be mock xconf url");
			LOGGER.info("******************************************************************************");
			errorMessage = "Failed to update the parameter Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.FWUpdate.AutoExcluded. as true and mock url using webpa";
			status = BroadBandWebPaUtils.setVerifyWebPAInPolledDuration(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_FWUPDATE_AUTO_EXCLUDED_ENABLE,
					BroadBandTestConstants.CONSTANT_3, BroadBandTestConstants.TRUE,
					BroadBandTestConstants.ONE_MINUTE_IN_MILLIS, BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS)
					&& BroadBandWebPaUtils.setVerifyWebPAInPolledDuration(device, tapEnv,
							BroadBandWebPaConstants.WEBPA_PARAM_FWUPDATE_AUTO_EXCLCUDED_XCONF_URL,
							BroadBandTestConstants.CONSTANT_0, XConfUtils.getXconfServerUrl(device),
							BroadBandTestConstants.ONE_MINUTE_IN_MILLIS,
							BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
			if (status) {
				LOGGER.info("STEP " + stepNumber
						+ ": ACTUAL: Successfully updated auto excluded enable as true and XconfUrl as mock url");
			} else {
				LOGGER.error("STEP " + stepNumber + ": ACTUAL: " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			/**
			 * Step 3 : Set credEnable to true and CredUse to true using webpa/dmcli
			 */
			stepNum = "s" + (++stepNumber);
			errorMessage = "Failed to enable cred dwnld parameters using webpa/dmcli";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP" + (stepNumber)
					+ ": DESCRIPTION : Set credEnable to true and CredUse to true using webpa/dmcli");
			LOGGER.info("STEP " + (stepNumber)
					+ ": ACTION : Execute webpa parameter:1.Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.CredDwnld.Enable2.Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.CredDwnld.Use");
			LOGGER.info("STEP " + (stepNumber) + ": EXPECTED : cred dwnld parameters should enabled using webpa/dmcli");
			LOGGER.info("**********************************************************************************");
			if (BroadBandWebPaUtils.setParameterValuesUsingWebPaOrDmcli(device, tapEnv,
					BroadBandWebPaConstants.TR181_PARAM_CRED_DWNLD_ENABLE, BroadBandTestConstants.CONSTANT_3,
					BroadBandTestConstants.TRUE)) {
				status = BroadBandWebPaUtils.setParameterValuesUsingWebPaOrDmcli(device, tapEnv,
						BroadBandWebPaConstants.TR181_PARAM_CRED_USE_ENABLE, BroadBandTestConstants.CONSTANT_0,
						BroadBandTestConstants.TRUE);
			}
			if (status) {
				LOGGER.info("STEP " + (stepNumber)
						+ ": ACTUAL : Successfully updated cred dwnld parameter values using webpa/dmcli");
			} else {
				LOGGER.error("STEP " + (stepNumber) + ": ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			Boolean isSupportedDevices = null;

			isSupportedDevices = BroadbandPropertyFileHandler.isDeviceSupported(device);

			if (isSupportedDevices || CommonMethods.isAtomSyncAvailable(device, tapEnv)) {

				/**
				 * STEP 4 : Enable Wifi Blaster Feature Using Webpa
				 */
				stepNum = "s" + (++stepNumber);
				status = false;
				LOGGER.info("******************************************************************************");
				LOGGER.info("STEP " + stepNumber + ": DESCRIPTION: Enable Wifi Blaster Feature Using Webpa");
				LOGGER.info("STEP " + stepNumber
						+ ": ACTION: Execute webpa command: tr181.Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.WifiClient.ActiveMeasurements.Enable");
				LOGGER.info("STEP " + stepNumber + ": EXPECTED: Wifi Blaster feature must be enabled by Webpa");
				LOGGER.info("******************************************************************************");
				errorMessage = "Wifi Blaster Feature is not enabled using Webpa";
				status = BroadBandWebPaUtils.setAndVerifyParameterValuesUsingWebPaorDmcli(device, tapEnv,
						BroadBandWebPaConstants.WEBPA_PARAM_FOR_RFC_WIFI_BLASTER_ENABLE,
						WebPaDataTypes.BOOLEAN.getValue(), BroadBandTestConstants.TRUE,
						BroadBandTestConstants.TEN_SECOND_IN_MILLIS);
				if (status) {
					LOGGER.info("STEP " + stepNumber + ": ACTUAL: Wifi Blaster feature is enabled by Webpa");
				} else {
					LOGGER.error("STEP " + stepNumber + ": ACTUAL: " + errorMessage);
				}
				LOGGER.info("******************************************************************************");
				tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

				/**
				 * STEP 5 : Enable Wifi Blaster trigger paramUsing Webpa
				 */
				stepNum = "s" + (++stepNumber);
				status = false;
				LOGGER.info("******************************************************************************");
				LOGGER.info("STEP " + stepNumber + ": DESCRIPTION: Enable Wifi Blaster trigger param using webpa");
				LOGGER.info("STEP " + stepNumber
						+ ": ACTION: Execute webpa command: tr181.Device.WiFi.X_RDKCENTRAL-COM_Report.WifiClient.ActiveMeasurements.Enable");
				LOGGER.info("STEP " + stepNumber
						+ ": EXPECTED: Wifi Blaster trigger param must be disabled on setting to true by Webpa");
				LOGGER.info("******************************************************************************");
				errorMessage = "Wifi Blaster trigger param is not enabled using Webpa";
				status = BroadBandWebPaUtils.setParameterValuesUsingWebPaOrDmcli(device, tapEnv,
						BroadBandWebPaConstants.WEBPA_PARAM_FOR_WIFI_BLASTER_TRIGGER, WebPaDataTypes.BOOLEAN.getValue(),
						BroadBandTestConstants.TRUE);
				if (status) {
					status = BroadBandWebPaUtils.getAndVerifyWebpaValueInPolledDuration(device, tapEnv,
							BroadBandWebPaConstants.WEBPA_PARAM_FOR_WIFI_BLASTER_TRIGGER, BroadBandTestConstants.TRUE,
							BroadBandTestConstants.ONE_MINUTE_IN_MILLIS,
							BroadBandTestConstants.FIFTEEN_SECONDS_IN_MILLIS);
				}
				if (status) {
					LOGGER.info("STEP " + stepNumber
							+ ": ACTUAL: Wifi Blaster trigger param is disabled after set to true by Webpa");
				} else {
					LOGGER.error("STEP " + stepNumber + ": ACTUAL: " + errorMessage);
				}
				LOGGER.info("******************************************************************************");
				tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

				/**
				 * STEP 6 : Set and Verify Wifi Blaster PacketSize Parameter to 1000
				 */
				stepNum = "s" + (++stepNumber);
				status = false;
				LOGGER.info("******************************************************************************");
				LOGGER.info("STEP " + stepNumber
						+ ": DESCRIPTION: Set and Verify Wifi Blaster PacketSize Parameter to 1000");
				LOGGER.info("STEP " + stepNumber
						+ ": ACTION: Execute webpa command: tr181.Device.WiFi.X_RDKCENTRAL-COM_Report.WifiClient.ActiveMeasurements.PacketSize uint 1000");
				LOGGER.info("STEP " + stepNumber + ": EXPECTED: Wifi Blaster PacketSize Parameter must be set to 1000");
				LOGGER.info("******************************************************************************");
				errorMessage = "Wifi Blaster PacketSize Parameter is not set to 1000";
				status = BroadBandWebPaUtils.setAndVerifyParameterValuesUsingWebPaorDmcli(device, tapEnv,
						BroadBandWebPaConstants.WEBPA_PARAM_FOR_BLASTER_PACKETSIZE, BroadBandTestConstants.CONSTANT_2,
						BroadBandTestConstants.STRING_CONSTANT_1000, BroadBandTestConstants.TEN_SECOND_IN_MILLIS);
				if (status) {
					LOGGER.info("STEP " + stepNumber + ": ACTUAL: Wifi Blaster PacketSize Parameter is set to 1000");
				} else {
					LOGGER.error("STEP " + stepNumber + ": ACTUAL: " + errorMessage);
				}
				LOGGER.info("******************************************************************************");
				tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

				/**
				 * STEP 7 : Set and Verify Wifi Blaster SampleDuration Parameter to 5000
				 */
				stepNum = "s" + (++stepNumber);
				status = false;
				LOGGER.info("******************************************************************************");
				LOGGER.info("STEP " + stepNumber
						+ ": DESCRIPTION: Set and Verify Wifi Blaster SampleDuration Parameter to 5000");
				LOGGER.info("STEP " + stepNumber
						+ ": ACTION: Execute webpa command: tr181.Device.WiFi.X_RDKCENTRAL-COM_Report.WifiClient.ActiveMeasurements.SampleDuration uint 5000");
				LOGGER.info(
						"STEP " + stepNumber + ": EXPECTED: Wifi Blaster SampleDuration Parameter must be set to 5000");
				LOGGER.info("******************************************************************************");
				errorMessage = "Wifi Blaster SampleDuration Parameter is not set to 5000";
				status = BroadBandWebPaUtils.setAndVerifyParameterValuesUsingWebPaorDmcli(device, tapEnv,
						BroadBandWebPaConstants.WEBPA_PARAM_FOR_BLASTER_SAMPLEDURATION,
						BroadBandTestConstants.CONSTANT_2, BroadBandTestConstants.STRING_CONSTANT_5000,
						BroadBandTestConstants.TEN_SECOND_IN_MILLIS);
				if (status) {
					LOGGER.info(
							"STEP " + stepNumber + ": ACTUAL: Wifi Blaster SampleDuration Parameter is set to 5000");
				} else {
					LOGGER.error("STEP " + stepNumber + ": ACTUAL: " + errorMessage);
				}
				LOGGER.info("******************************************************************************");
				tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

				/**
				 * STEP 8 : Set and Verify Wifi Blaster NumberOfSamples Parameter to 90
				 */
				stepNum = "s" + (++stepNumber);
				status = false;
				LOGGER.info("******************************************************************************");
				LOGGER.info("STEP " + stepNumber
						+ ": DESCRIPTION: Set and Verify Wifi Blaster NumberOfSamples Parameter to 90");
				LOGGER.info("STEP " + stepNumber
						+ ": ACTION: Execute webpa command: tr181.Device.WiFi.X_RDKCENTRAL-COM_Report.WifiClient.ActiveMeasurements.NumberOfSamples uint 90");
				LOGGER.info(
						"STEP " + stepNumber + ": EXPECTED: Wifi Blaster NumberOfSamples Parameter must be set to 90");
				LOGGER.info("******************************************************************************");
				errorMessage = "Wifi Blaster NumberOfSamples Parameter is not set to 90";
				status = BroadBandWebPaUtils.setAndVerifyParameterValuesUsingWebPaorDmcli(device, tapEnv,
						BroadBandWebPaConstants.WEBPA_PARAM_FOR_BLASTER_NUMOFSAMPLES, BroadBandTestConstants.CONSTANT_2,
						BroadBandTestConstants.STRING_VALUE_90, BroadBandTestConstants.TEN_SECOND_IN_MILLIS);
				if (status) {
					LOGGER.info(
							"STEP " + stepNumber + ": ACTUAL: Wifi Blaster NumberOfSamples Parameter is set to 5000");
				} else {
					LOGGER.error("STEP " + stepNumber + ": ACTUAL: " + errorMessage);
				}
				LOGGER.info("******************************************************************************");
				tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

				/** STEP 9 : Set and Verify Wifi Blaster PlanID Parameter to */
				stepNum = "s" + (++stepNumber);
				status = false;
				LOGGER.info("******************************************************************************");
				LOGGER.info("STEP " + stepNumber
						+ ": DESCRIPTION: Set and Verify Wifi Blaster PlanID Parameter");
				LOGGER.info("STEP " + stepNumber
						+ ": ACTION: Execute webpa command using tr181.Device.WiFi.X_RDKCENTRAL-COM_Report.WifiClient.ActiveMeasurements.Plan.PlanID");
				LOGGER.info("STEP " + stepNumber
						+ ": EXPECTED: Wifi Blaster PlanID Parameter must be set to ");
				LOGGER.info("******************************************************************************");
				errorMessage = "Wifi Blaster PlanID Parameter is not set";
				status = BroadBandWebPaUtils.setAndVerifyParameterValuesUsingWebPaorDmcli(device, tapEnv,
						BroadBandWebPaConstants.WEBPA_PARAM_FOR_BLASTER_PLANID, BroadBandTestConstants.CONSTANT_0,
						BroadBandTestConstants.STRING_BLASTER_PLANID, BroadBandTestConstants.TEN_SECOND_IN_MILLIS);
				if (status) {
					LOGGER.info("STEP " + stepNumber
							+ ": ACTUAL: Wifi Blaster PlanID Parameter is set");
				} else {
					LOGGER.error("STEP " + stepNumber + ": ACTUAL: " + errorMessage);
				}
				LOGGER.info("******************************************************************************");
				tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

				/**
				 * STEP 10 : Set and Verify Wifi Blaster stepID Parameter to 30
				 */
				stepNum = "s" + (++stepNumber);
				status = false;
				LOGGER.info("******************************************************************************");
				LOGGER.info("STEP " + stepNumber + ": DESCRIPTION: Set and Verify Wifi Blaster StepID Parameter to 30");
				LOGGER.info("STEP " + stepNumber
						+ ": ACTION: Execute webpa command: tr181.Device.WiFi.X_RDKCENTRAL-COM_Report.WifiClient.ActiveMeasurements.Plan.Step.1.StepID uint 30");
				LOGGER.info("STEP " + stepNumber + ": EXPECTED: Wifi Blaster StepID Parameter must be set to 30");
				LOGGER.info("******************************************************************************");
				errorMessage = "Wifi Blaster StepID Parameter is not set to 30";
				status = BroadBandWebPaUtils.setAndVerifyParameterValuesUsingWebPaorDmcli(device, tapEnv,
						(BroadBandWebPaConstants.WEBPA_PARAM_FOR_BLASTER_STEPID.replace(
								BroadBandTestConstants.TR181_NODE_REF, BroadBandTestConstants.STRING_VALUE_ONE)),
						BroadBandTestConstants.CONSTANT_2, BroadBandTestConstants.STRING_30,
						BroadBandTestConstants.TEN_SECOND_IN_MILLIS);
				if (status) {
					LOGGER.info("STEP " + stepNumber + ": ACTUAL: Wifi Blaster StepID Parameter is set to 30");
				} else {
					LOGGER.error("STEP " + stepNumber + ": ACTUAL: " + errorMessage);
				}
				LOGGER.info("******************************************************************************");
				tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

				/**
				 * STEP 11 : Set and Verify Wifi Blaster stepID SourcMac Parameter to device CM
				 * Mac
				 */
				stepNum = "s" + (++stepNumber);
				status = false;
				LOGGER.info("******************************************************************************");
				LOGGER.info("STEP " + stepNumber
						+ ": DESCRIPTION: Set and Verify Wifi Blaster SourceMac Parameter to ecm mac");
				LOGGER.info("STEP " + stepNumber
						+ ": ACTION: Execute webpa command: tr181.Device.WiFi.X_RDKCENTRAL-COM_Report.WifiClient.ActiveMeasurements.Plan.Step.1.SourceMac string ecmmac");
				LOGGER.info("STEP " + stepNumber + ": EXPECTED: Wifi Blaster SorceMac Parameter must be set to ecmmac");
				LOGGER.info("******************************************************************************");
				errorMessage = "Wifi Blaster SorceMac Parameter is not set to ecmmac";
				String ecmMac = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcli(device, tapEnv,
						BroadBandWebPaConstants.WEBPA_PARAM_CM_MAC);
				ecmMac = ecmMac.replace(BroadBandTestConstants.DELIMITER_COLON, BroadBandTestConstants.EMPTY_STRING);
				status = BroadBandWebPaUtils.setAndVerifyParameterValuesUsingWebPaorDmcli(device, tapEnv,
						(BroadBandWebPaConstants.WEBPA_PARAM_FOR_BLASTER_SOURCEMAC.replace(
								BroadBandTestConstants.TR181_NODE_REF, BroadBandTestConstants.STRING_VALUE_ONE)),
						BroadBandTestConstants.CONSTANT_0, ecmMac, BroadBandTestConstants.TEN_SECOND_IN_MILLIS);
				if (status) {
					LOGGER.info("STEP " + stepNumber + ": ACTUAL: Wifi Blaster SourceMac Parameter is set to ecmmac");
				} else {
					LOGGER.error("STEP " + stepNumber + ": ACTUAL: " + errorMessage);
				}
				LOGGER.info("******************************************************************************");
				tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

				/**
				 * STEP 12 : Set and Verify Wifi Blaster steID DestMac Parameter
				 */
				stepNum = "s" + (++stepNumber);
				status = false;
				LOGGER.info("******************************************************************************");
				LOGGER.info("STEP " + stepNumber
						+ ": DESCRIPTION: Set and Verify Wifi Blaster DestMac Parameter");
				LOGGER.info("STEP " + stepNumber
						+ ": ACTION: Execute webpa command using tr181.Device.WiFi.X_RDKCENTRAL-COM_Report.WifiClient.ActiveMeasurements.Plan.Step.1.DestMac");
				LOGGER.info("STEP " + stepNumber
						+ ": EXPECTED: Wifi Blaster DestMac Parameter must be set");
				LOGGER.info("******************************************************************************");
				errorMessage = "Wifi Blaster DestMac Parameter is not set";
				status = BroadBandWebPaUtils.setAndVerifyParameterValuesUsingWebPaorDmcli(device, tapEnv,
						(BroadBandWebPaConstants.WEBPA_PARAM_FOR_BLASTER_DESTMAC.replace(
								BroadBandTestConstants.TR181_NODE_REF, BroadBandTestConstants.STRING_VALUE_ONE)),
						BroadBandTestConstants.CONSTANT_0, BroadBandTestConstants.STRING_BLASTER_DESTMAC,
						BroadBandTestConstants.TEN_SECOND_IN_MILLIS);
				if (status) {
					LOGGER.info(
							"STEP " + stepNumber + ": ACTUAL: Wifi Blaster DestMac Parameter is set");
				} else {
					LOGGER.error("STEP " + stepNumber + ": ACTUAL: " + errorMessage);
				}
				LOGGER.info("**********************************************************************************");
				tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);
			} else {
				for (int num = BroadBandTestConstants.CONSTANT_4; num <= BroadBandTestConstants.CONSTANT_12; num++) {
					stepNum = "s" + num;
					tapEnv.updateExecutionForAllStatus(device, testCaseId, stepNum, ExecutionStatus.NOT_APPLICABLE,
							"step is applicable for specific Platforms only", false);
				}
			}

			/**
			 * STEP 13 :Update force wifi disable value as true using webpa
			 */
			stepNumber = BroadBandTestConstants.CONSTANT_12;
			stepNum = "s" + (++stepNumber);
			errorMessage = "Failed to set value of force disable wifi parameter to true";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP " + stepNumber + ": DESCRIPTION : Update force wifi disable value as true using webpa");
			LOGGER.info("STEP " + stepNumber
					+ ": ACTION : Execute webpa or dmcli command to set value of Device.WiFi.X_RDK-CENTRAL_COM_ForceDisable to true");
			LOGGER.info("STEP " + stepNumber + ": EXPECTED : EXPECTED : Successfully set parameter value to true");
			LOGGER.info("**********************************************************************************");
			status = BroadBandWebPaUtils.setAndVerifyParameterValuesUsingWebPaorDmcli(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_FORCE_WIFI_DISABLE, BroadBandTestConstants.CONSTANT_3,
					BroadBandTestConstants.TRUE);
			if (status) {
				LOGGER.info("STEP " + stepNumber + ": ACTUAL : Successfully set parameter value to true");
			} else {
				LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			/**
			 * STEP 14 : Reboot the device
			 */
			stepNum = "s" + (++stepNumber);
			errorMessage = "Device not coming up after reboot";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP " + stepNumber + ": DESCRIPTION : Reboot the device");
			LOGGER.info("STEP " + stepNumber + ": ACTION : Execute command: /sbin/reboot");
			LOGGER.info("STEP " + stepNumber + ": EXPECTED : Device should rebooted and comes online");
			LOGGER.info("**********************************************************************************");
			status = CommonMethods.rebootAndWaitForIpAccusition(device, tapEnv);
			if (status) {
				LOGGER.info("STEP " + stepNumber + ": ACTUAL : Device came online after reboot");
			} else {
				LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			/**
			 * STEP 15 : Verify DNS cache size is persist after reboot using webpa
			 */
			if (!isBusinessClsDevice) {
				stepNum = "s" + (++stepNumber);
				errorMessage = "DNS cache size is not persisted over reboot";
				status = false;
				LOGGER.info("**********************************************************************************");
				LOGGER.info("STEP " + stepNumber
						+ ": DESCRIPTION : Verify DNS cache size is persist after reboot using webpa");
				LOGGER.info("STEP " + stepNumber
						+ ": ACTION : Execute webpa command: Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.RabidFramework.DNSCacheSize");
				LOGGER.info("STEP " + stepNumber + ": EXPECTED : Value should persist after reboot ");
				LOGGER.info("**********************************************************************************");
				status = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcliAndVerify(device, tapEnv,
						BroadBandWebPaConstants.WEBPA_PARAM_RABID_DNS_CACHE_SIZE, BroadBandTestConstants.STRING_15000);
				if (status) {
					LOGGER.info("STEP " + stepNumber
							+ ": ACTUAL : Succesfully verified DNS cache size persisted over reboot.");
				} else {
					LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
				}
				LOGGER.info("**********************************************************************************");
				tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);
			} else {
				stepNum = "S" + (++stepNumber);
				LOGGER.info("STEP " + stepNumber
						+ " : ACTUAL : Verify DNS cache size is persist after reboot is not applicable for business class devices");
				LOGGER.info("**********************************************************************************");
				tapEnv.updateExecutionForAllStatus(device, testCaseId, stepNum, ExecutionStatus.NOT_APPLICABLE,
						BroadBandTestConstants.NA_MSG_FOR_BUSINESS_CLASS_DEVICES, false);
			}

			/**
			 * STEP 16 : Verify Auto exclude enable status and xconf url using webpa after
			 * reboot
			 */
			stepNum = "s" + (++stepNumber);
			errorMessage = "Auto exclude enable status and xconf url is not persisted after reboot";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP " + stepNumber
					+ ": DESCRIPTION : Verify Auto exclude enable status and xconf url using webpa after reboot");
			LOGGER.info("STEP " + stepNumber
					+ ": ACTION : Execute webpa command: Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.FWUpdate.AutoExcluded.Enable,Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.FWUpdate.AutoExcluded.XconfUrl");
			LOGGER.info("STEP " + stepNumber
					+ ": EXPECTED : Auto exclude status and xconf url should persist after reboot");
			LOGGER.info("**********************************************************************************");
			status = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcliAndVerify(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_FWUPDATE_AUTO_EXCLUDED_ENABLE, BroadBandTestConstants.TRUE)
					&& BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcliAndVerify(device, tapEnv,
							BroadBandWebPaConstants.WEBPA_PARAM_FWUPDATE_AUTO_EXCLCUDED_XCONF_URL,
							XConfUtils.getXconfServerUrl(device));
			if (status) {
				LOGGER.info("STEP " + stepNumber
						+ ": ACTUAL : Succesfully verified auto exclude status and exclude url values persisted over reboot.");
			} else {
				LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			/**
			 * STEP 17 : Verify cred dwnld enable is true and use is true using webpa
			 */
			stepNum = "s" + (++stepNumber);
			errorMessage = "cred dwnld parameters value not persisted after reboot";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP " + stepNumber
					+ ": DESCRIPTION : Verify cred dwnld enable is true and use is true using webpa");
			LOGGER.info("STEP " + stepNumber
					+ ": ACTION : Execute webpa parameter: 1.Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.CredDwnld.Enable "
					+ "2.Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.CredDwnld.Use");
			LOGGER.info("STEP " + stepNumber
					+ ": EXPECTED : cred dwnld enable should return true and use should return true");
			LOGGER.info("**********************************************************************************");
			if (BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcliAndVerify(device, tapEnv,
					BroadBandWebPaConstants.TR181_PARAM_CRED_DWNLD_ENABLE, BroadBandTestConstants.TRUE)) {
				status = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcliAndVerify(device, tapEnv,
						BroadBandWebPaConstants.TR181_PARAM_CRED_USE_ENABLE, BroadBandTestConstants.TRUE);
			}
			if (status) {
				LOGGER.info("STEP " + stepNumber
						+ ": ACTUAL : Successfully verified cred dwnld parameters value persists after reboot.");
			} else {
				LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			isSupportedDevices = null;

			isSupportedDevices = BroadbandPropertyFileHandler.isDeviceSupported(device);

			if (isSupportedDevices || CommonMethods.isAtomSyncAvailable(device, tapEnv)) {

				/**
				 * STEP 18 : Verify Wifi Balster feature is enabled after reboot
				 */
				stepNum = "s" + (++stepNumber);
				status = false;
				LOGGER.info("**********************************************************************************");
				LOGGER.info(
						"STEP " + stepNumber + ": DESCRIPTION : Verify Wifi Balster feature is enabled after reboot");
				LOGGER.info("STEP " + stepNumber
						+ ": ACTION : Execute webpa command: Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.WifiClient.ActiveMeasurements.Enable");
				LOGGER.info("STEP " + stepNumber + ": EXPECTED : Wifi Balster feature must be enabled after reboot ");
				LOGGER.info("**********************************************************************************");
				errorMessage = "wifi Blaster feature is not enabled after reboot";
				status = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcliAndVerify(device, tapEnv,
						BroadBandWebPaConstants.WEBPA_PARAM_FOR_RFC_WIFI_BLASTER_ENABLE, BroadBandTestConstants.TRUE);
				if (status) {
					LOGGER.info("STEP " + stepNumber + ": ACTUAL : Wifi Balster feature is enabled after reboot .");
				} else {
					LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
				}
				LOGGER.info("**********************************************************************************");
				tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

				/**
				 * STEP 19 : Verify Wifi Balster trigger param is enabled after reboot
				 */
				stepNum = "s" + (++stepNumber);
				status = false;
				LOGGER.info("**********************************************************************************");
				LOGGER.info("STEP " + stepNumber
						+ ": DESCRIPTION : Verify Wifi Blaster trigger param is enabled after reboot");
				LOGGER.info("STEP " + stepNumber
						+ ": ACTION : Execute webpa command: Device.WiFi.X_RDKCENTRAL-COM_Report.WifiClient.ActiveMeasurements.Enable");
				LOGGER.info(
						"STEP " + stepNumber + ": EXPECTED : Wifi Balster trigger param must be disabled on reboot ");
				LOGGER.info("**********************************************************************************");
				errorMessage = "wifi Blaster trigger param is not enabled after reboot";
				status = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcliAndVerify(device, tapEnv,
						BroadBandWebPaConstants.WEBPA_PARAM_FOR_WIFI_BLASTER_TRIGGER, BroadBandTestConstants.TRUE);
				if (status) {
					LOGGER.info(
							"STEP " + stepNumber + ": ACTUAL : Wifi Balster trigger param is disabled after reboot .");
				} else {
					LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
				}
				LOGGER.info("**********************************************************************************");
				tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

				/**
				 * STEP 20 : Verify Wifi Balster PacketSize parameter is persisting after reboot
				 */
				stepNum = "s" + (++stepNumber);
				status = false;
				LOGGER.info("**********************************************************************************");
				LOGGER.info("STEP " + stepNumber
						+ ": DESCRIPTION : Verify Wifi Blaster PacketSize parameter is persisting  after reboot");
				LOGGER.info("STEP " + stepNumber
						+ ": ACTION : Execute webpa command: Device.WiFi.X_RDKCENTRAL-COM_Report.WifiClient.ActiveMeasurements.PacketSize");
				LOGGER.info("STEP " + stepNumber
						+ ": EXPECTED : wifi Balster PacketSize parameter must be 1000 after reboot");
				LOGGER.info("**********************************************************************************");
				errorMessage = "wifi Balster PacketSize parameter is not 1000 after reboot";
				status = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcliAndVerify(device, tapEnv,
						BroadBandWebPaConstants.WEBPA_PARAM_FOR_BLASTER_PACKETSIZE,
						BroadBandTestConstants.STRING_CONSTANT_1000);
				if (status) {
					LOGGER.info(
							"STEP " + stepNumber + ": ACTUAL : wifi Balster PacketSize parameter is 1000 after reboot");
				} else {
					LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
				}
				LOGGER.info("**********************************************************************************");
				tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

				/**
				 * STEP 21 : Verify Wifi Balster SampleDuration parameter is persisting after
				 * reboot
				 */
				stepNum = "s" + (++stepNumber);
				status = false;
				LOGGER.info("**********************************************************************************");
				LOGGER.info("STEP " + stepNumber
						+ ": DESCRIPTION : Verify Wifi Balster SampleDuration parameter is persisting  after reboot");
				LOGGER.info("STEP " + stepNumber
						+ ": ACTION : Execute webpa command: Device.WiFi.X_RDKCENTRAL-COM_Report.WifiClient.ActiveMeasurements.SampleDuration");
				LOGGER.info("STEP " + stepNumber
						+ ": EXPECTED : wifi Balster SampleDuration parameter must be 5000 after reboot");
				LOGGER.info("**********************************************************************************");
				errorMessage = "wifi Balster SampleDuration parameter is not 5000 after reboot";
				status = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcliAndVerify(device, tapEnv,
						BroadBandWebPaConstants.WEBPA_PARAM_FOR_BLASTER_SAMPLEDURATION,
						BroadBandTestConstants.STRING_CONSTANT_5000);
				if (status) {
					LOGGER.info("STEP " + stepNumber
							+ ": ACTUAL : wifi Balster SampleDuration parameter is 5000 after reboot");
				} else {
					LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
				}
				LOGGER.info("**********************************************************************************");
				tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

				/**
				 * STEP 22 : Verify Wifi Balster NumberOfSamples parameter is persisting after
				 * reboot
				 */
				stepNum = "s" + (++stepNumber);
				status = false;
				LOGGER.info("**********************************************************************************");
				LOGGER.info("STEP " + stepNumber
						+ ": DESCRIPTION : Verify Wifi Balster NumberOfSamples parameter is persisting  after reboot");
				LOGGER.info("STEP " + stepNumber
						+ ": ACTION : Execute webpa command: Device.WiFi.X_RDKCENTRAL-COM_Report.WifiClient.ActiveMeasurements.NumberOfSamples");
				LOGGER.info("STEP " + stepNumber
						+ ": EXPECTED : wifi Balster NumberOfSamples parameter must be 90 after reboot");
				LOGGER.info("**********************************************************************************");
				errorMessage = "wifi Balster NumberOfSamples parameter is not 90 after reboot";
				status = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcliAndVerify(device, tapEnv,
						BroadBandWebPaConstants.WEBPA_PARAM_FOR_BLASTER_NUMOFSAMPLES,
						BroadBandTestConstants.STRING_VALUE_90);
				if (status) {
					LOGGER.info("STEP " + stepNumber
							+ ": ACTUAL : wifi Balster NumberOfSamples parameter is 90 after reboot");
				} else {
					LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
				}
				LOGGER.info("**********************************************************************************");
				tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

				/**
				 * STEP 23 : Verify Wifi Balster PlanID parameter is not persisting after reboot
				 */
				stepNum = "s" + (++stepNumber);
				status = false;
				LOGGER.info("**********************************************************************************");
				LOGGER.info("STEP " + stepNumber
						+ ": DESCRIPTION : Verify Wifi Balster PlanID parameter is not persisting  after reboot");
				LOGGER.info("STEP " + stepNumber
						+ ": ACTION : Execute webpa command: Device.WiFi.X_RDKCENTRAL-COM_Report.WifiClient.ActiveMeasurements.Plan.PlanID");
				LOGGER.info("STEP " + stepNumber
						+ ": EXPECTED : wifi Balster PlanID parameter must be blank/empty after reboot");
				LOGGER.info("**********************************************************************************");
				errorMessage = "wifi Balster PlanID parameter is not blank/empty after reboot";

				try {
					response = tapEnv.executeWebPaCommand(device,
							BroadBandWebPaConstants.WEBPA_PARAM_FOR_BLASTER_PLANID);
				} catch (Exception e) {
					LOGGER.error("Exception caught while retrieving value using webpa: " + e.getMessage());
				}
				status = (response != null && response.length() == 0);
				if (status) {
					LOGGER.info("STEP " + stepNumber
							+ ": ACTUAL : wifi Balster PlanID parameter is blank/empty after reboot");
				} else {
					LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
				}
				LOGGER.info("**********************************************************************************");
				tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

				/**
				 * STEP 24 : Verify Wifi Balster StepID parameter is not persisting after reboot
				 */
				stepNum = "s" + (++stepNumber);
				status = false;
				LOGGER.info("**********************************************************************************");
				LOGGER.info("STEP " + stepNumber
						+ ": DESCRIPTION : Verify Wifi Balster StepID parameter is not persisting  after reboot");
				LOGGER.info("STEP " + stepNumber
						+ ": ACTION : Execute webpa command: Device.WiFi.X_RDKCENTRAL-COM_Report.WifiClient.ActiveMeasurements.Plan.Step.1.StepID");
				LOGGER.info("STEP " + stepNumber + ": EXPECTED : Wifi Blaster StepID Parameter must be 0 after reboot");
				LOGGER.info("**********************************************************************************");
				errorMessage = "Wifi Blaster StepID Parameter is not Null";
				status = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcliAndVerify(device, tapEnv,
						BroadBandWebPaConstants.WEBPA_PARAM_FOR_BLASTER_STEPID.replace(
								BroadBandTestConstants.TR181_NODE_REF, BroadBandTestConstants.STRING_VALUE_ONE),
						BroadBandTestConstants.STRING_VALUE_ZERO);
				if (status) {
					LOGGER.info("STEP " + stepNumber + ": ACTUAL : wifi Balster StepID parameter is 0 after reboot");
				} else {
					LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
				}
				LOGGER.info("**********************************************************************************");
				tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

				/**
				 * STEP 25 : Verify Wifi Balster SourceMac parameter is not persisting after
				 * reboot
				 */
				stepNum = "s" + (++stepNumber);
				status = false;
				LOGGER.info("**********************************************************************************");
				LOGGER.info("STEP " + stepNumber
						+ ": DESCRIPTION : Verify Wifi Balster SourceMac parameter is not persisting  after reboot");
				LOGGER.info("STEP " + stepNumber
						+ ": ACTION : Execute webpa command: Execute webPA command:Device.WiFi.X_RDKCENTRAL-COM_Report.WifiClient.ActiveMeasurements.Plan.Step.1.SourceMac");
				LOGGER.info("STEP " + stepNumber
						+ ": EXPECTED : Wifi Blaster StepID SourceMac Parameter must be blank/empty after reboot");
				LOGGER.info("**********************************************************************************");
				errorMessage = "Wifi Blaster StepID SourceMac Parameter is not blank/empty after reboot";

				try {
					response = tapEnv.executeWebPaCommand(device,
							BroadBandWebPaConstants.WEBPA_PARAM_FOR_BLASTER_SOURCEMAC.replace(
									BroadBandTestConstants.TR181_NODE_REF, BroadBandTestConstants.STRING_VALUE_ONE));
				} catch (Exception e) {
					LOGGER.error("Exception caught while retrieving value using webpa: " + e.getMessage());
				}
				status = (response != null && response.length() == 0);
				if (status) {
					LOGGER.info("STEP " + stepNumber
							+ ": ACTUAL : Wifi Blaster StepID SourceMac Parameter is blank/empty after reboot");
				} else {
					LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
				}
				LOGGER.info("**********************************************************************************");
				tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

				/**
				 * STEP 26 : Verify Wifi Balster DestMac parameter is not persisting after
				 * reboot
				 */
				stepNum = "s" + (++stepNumber);
				status = false;
				LOGGER.info("**********************************************************************************");
				LOGGER.info("STEP " + stepNumber
						+ ": DESCRIPTION : Verify Wifi Balster DestMac parameter is not persisting  after reboot");
				LOGGER.info("STEP " + stepNumber
						+ ": ACTION : Execute webpa command: Execute webPA command:Device.WiFi.X_RDKCENTRAL-COM_Report.WifiClient.ActiveMeasurements.Plan.Step.1.DestMac");
				LOGGER.info("STEP " + stepNumber
						+ ": EXPECTED : Wifi Blaster StepID DestMac Parameter must be blank/empty after reboot");
				LOGGER.info("**********************************************************************************");
				errorMessage = "Wifi Blaster StepID DestMac Parameter is not blank/empty after reboot";
				try {
					response = tapEnv.executeWebPaCommand(device,
							BroadBandWebPaConstants.WEBPA_PARAM_FOR_BLASTER_DESTMAC.replace(
									BroadBandTestConstants.TR181_NODE_REF, BroadBandTestConstants.STRING_VALUE_ONE));
				} catch (Exception e) {
					LOGGER.error("Exception caught while retrieving value using webpa: " + e.getMessage());
				}
				status = (response != null && response.length() == 0);
				if (status) {
					LOGGER.info("STEP " + stepNumber
							+ ": ACTUAL : Wifi Blaster StepID DestMac Parameter is blank/empty after reboot");
				} else {
					LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
				}
				LOGGER.info("**********************************************************************************");
				tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);
			} else {
				for (int num = BroadBandTestConstants.CONSTANT_18; num <= BroadBandTestConstants.CONSTANT_26; num++) {
					stepNum = "s" + num;
					tapEnv.updateExecutionForAllStatus(device, testCaseId, stepNum, ExecutionStatus.NOT_APPLICABLE,
							"step is applicable for specific Platforms only", false);
				}
			}

			/**
			 * STEP 27 : Verify force wifi disable value as true after reboot
			 */
			stepNumber = BroadBandTestConstants.CONSTANT_26;
			stepNum = "s" + (++stepNumber);
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP " + stepNumber + ": DESCRIPTION : Verify  force wifi disable value as true after reboot");
			LOGGER.info("STEP " + stepNumber
					+ ": ACTION : Execute webpa command: Execute webPA command:Device.WiFi.X_RDK-CENTRAL_COM_ForceDisable");
			LOGGER.info("STEP " + stepNumber + ": EXPECTED : paramter should respond value as true");
			LOGGER.info("**********************************************************************************");
			errorMessage = "force disable value is not persisted after reboot";
			status = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcliAndVerify(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_FORCE_WIFI_DISABLE, BroadBandTestConstants.TRUE);
			if (status) {
				LOGGER.info("STEP " + stepNumber + ": ACTUAL : force disable value is persisted after reboot");
			} else {
				LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			/**
			 * STEP 28 : Verify 2.4Ghz radio has been disabled
			 */
			stepNum = "s" + (++stepNumber);
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP " + stepNumber + ": DESCRIPTION : Verify 2.4Ghz radio has been disabled");
			LOGGER.info("STEP " + stepNumber
					+ ": ACTION : Execute webpa or dmcli command  to get value of Device.WiFi.Radio.1.Enable");
			LOGGER.info("STEP " + stepNumber + ": EXPECTED : Value of parameter should be set to false");
			LOGGER.info("**********************************************************************************");
			errorMessage = "Failed to verify value of 2.4G radio enable parameter as false after reboot";
			status = BroadBandWebPaUtils.getAndVerifyWebpaValueInPolledDuration(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_WIFI_2_4_RADIO_ENABLE, BroadBandTestConstants.FALSE,
					BroadBandTestConstants.THREE_MINUTE_IN_MILLIS, BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
			if (status) {
				LOGGER.info("STEP " + stepNumber + ": ACTUAL : 2.4Ghz radio enable is false after reboot");
			} else {
				LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			/**
			 * STEP 29 : Verify 5Ghz radio has been disabled
			 */
			stepNum = "s" + (++stepNumber);
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP " + stepNumber + ": DESCRIPTION : Verify 5Ghz radio has been disabled");
			LOGGER.info("STEP " + stepNumber
					+ ": ACTION : Execute webpa or dmcli command  to get value of Device.WiFi.Radio.2.Enable");
			LOGGER.info("STEP " + stepNumber + ": EXPECTED : Value of parameter should be set to false");
			LOGGER.info("**********************************************************************************");
			errorMessage = "Failed to verify value of 5G radio enable parameter as false after reboot";
			status = BroadBandWebPaUtils.getAndVerifyWebpaValueInPolledDuration(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_WIFI_5_RADIO_ENABLE, BroadBandTestConstants.FALSE,
					BroadBandTestConstants.THREE_MINUTE_IN_MILLIS, BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
			if (status) {
				LOGGER.info("STEP " + stepNumber + ": ACTUAL : 5Ghz radio enable is false after reboot");
			} else {
				LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			/**
			 * STEP 30 : Do factory reset the device using webpa
			 */
			stepNum = "s" + (++stepNumber);
			errorMessage = "Device not coming up after factory reboot.";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP " + stepNumber + ": DESCRIPTION : Do factory reset the device using webpa");
			LOGGER.info("STEP " + stepNumber
					+ ": ACTION : Execute webpa set command: parameter: Device.X_CISCO_COM_DeviceControl.FactoryReset datatype: string value: Router,Wifi,VoIP,Dect,MoCA");
			LOGGER.info("STEP " + stepNumber + ": EXPECTED : Device should be ssh able after factory reset");
			LOGGER.info("**********************************************************************************");
			status = BroadBandCommonUtils.performFactoryResetAndWaitForWebPaProcessToUp(tapEnv, device);
			if (status) {
				LOGGER.info("STEP " + stepNumber + ": ACTUAL : Devices came online after factory reset");
			} else {
				LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

			/**
			 * STEP 31 : Verify rabid dns cache size value set to default value of 10000
			 * after factory reset
			 */
			stepNum = "s" + (++stepNumber);
			errorMessage = "DNS cache size not changed to default value after factory reset";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP " + stepNumber
					+ ": DESCRIPTION : Verify rabid dns cache size value set to default value of 10000 after factory reset");
			LOGGER.info("STEP " + stepNumber
					+ ": ACTION : Execute webpa get command: parameter: Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.RabidFramework.DNSCacheSize");
			LOGGER.info("STEP " + stepNumber
					+ ": EXPECTED : DNS cache size value should be default as 10000 after factory reset");
			LOGGER.info("**********************************************************************************");
			if (!DeviceModeHandler.isBusinessClassDevice(device)) {
				status = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcliAndVerify(device, tapEnv,
						BroadBandWebPaConstants.WEBPA_PARAM_RABID_DNS_CACHE_SIZE, BroadBandTestConstants.STRING_10000);
				if (status) {
					LOGGER.info("STEP " + stepNumber
							+ ": ACTUAL : Successfully verified DNS cache size changed to default value after factory reset.");
				} else {
					LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
				}
				LOGGER.info("**********************************************************************************");
				tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);
			} else {
				LOGGER.info(
						"STEP :" + stepNumber + " : ACTUAL :  DNS CACHE SIZE IS NOT APPLICABLE FOR COMMERCIAL DEVICES");
				LOGGER.info("**********************************************************************************");
				tapEnv.updateExecutionForAllStatus(device, testCaseId, stepNum, ExecutionStatus.NOT_APPLICABLE,
						BroadBandTestConstants.NA_MSG_FOR_COMMERCIAL_DEVICES, false);
			}

			/**
			 * STEP 32 : Verify Auto exclude enable status and xconf url using webpa after
			 * factory reset
			 */
			stepNum = "s" + (++stepNumber);
			errorMessage = "Auto exclude enable status and xconf url is not having default value after factory reset";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP " + stepNumber
					+ ": DESCRIPTION : Verify Auto exclude enable status and xconf url using webpa after factory reset");
			LOGGER.info("STEP " + stepNumber
					+ ": ACTION : Execute webpa command: Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.FWUpdate.AutoExcluded.Enable,Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.FWUpdate.AutoExcluded.XconfUrl");
			LOGGER.info("STEP " + stepNumber
					+ ": EXPECTED : Auto exclude status and xconf url should be default value after factory reset");
			LOGGER.info("**********************************************************************************");
			if (BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcliAndVerify(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_FWUPDATE_AUTO_EXCLUDED_ENABLE, BroadBandTestConstants.FALSE)) {
				response = tapEnv.executeWebPaCommand(device,
						BroadBandWebPaConstants.WEBPA_PARAM_FWUPDATE_AUTO_EXCLCUDED_XCONF_URL);
				status = CommonMethods.isNotNull(response)
						&& response.equalsIgnoreCase("https://<url>/xconf/swu/stb");
			}
			if (status) {
				LOGGER.info("STEP " + stepNumber
						+ ": ACTUAL : Succesfully verified default values of auto exclude status and exclude url after factory reset");
			} else {
				LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			/**
			 * STEP 33 : Verify cred dwnld enable is false and use is false using
			 * webpa/dmcli
			 */
			stepNum = "s" + (++stepNumber);
			errorMessage = "After factory reset cred dwnld parameters not changed to default values";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP " + stepNumber
					+ ": DESCRIPTION : Verify cred dwnld enable is false and use is false using webpa/dmcli");
			LOGGER.info("STEP " + stepNumber
					+ ": ACTION : Execute webpa parameter: 1.Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.CredDwnld.Enable "
					+ "2.Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.CredDwnld.Use");
			LOGGER.info("STEP " + stepNumber
					+ ": EXPECTED : cred dwnld enable should return false and use should return false");
			LOGGER.info("**********************************************************************************");
			if (BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcliAndVerify(device, tapEnv,
					BroadBandWebPaConstants.TR181_PARAM_CRED_DWNLD_ENABLE, BroadBandTestConstants.FALSE)) {
				status = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcliAndVerify(device, tapEnv,
						BroadBandWebPaConstants.TR181_PARAM_CRED_USE_ENABLE, BroadBandTestConstants.FALSE);
			}
			if (status) {
				LOGGER.info("STEP " + stepNumber
						+ ": ACTUAL : Successfully verified cred dwnld parameters are disabled after factory reset");
			} else {
				LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			isSupportedDevices = null;

			isSupportedDevices = BroadbandPropertyFileHandler.isDeviceSupported(device);

			if (isSupportedDevices || CommonMethods.isAtomSyncAvailable(device, tapEnv)) {

				/**
				 * STEP 34 : Validate Wifi Blaster RFC parameter is disabled by default
				 */
				stepNum = "s" + (++stepNumber);
				status = false;
				LOGGER.info("**********************************************************************************");
				LOGGER.info("STEP " + stepNumber
						+ ": DESCRIPTION : Validate Wifi Blaster RFC parameter is disabled by default");
				LOGGER.info("STEP " + stepNumber
						+ ": ACTION : Execute webpa command: Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.WifiClient.ActiveMeasurements.Enable");
				LOGGER.info(
						"STEP " + stepNumber + ": EXPECTED : Wifi Blaster RFC parameter must be disabled by deafult ");
				LOGGER.info("**********************************************************************************");
				errorMessage = "Wifi Blaster RFC parameter is not disabled by deafult";
				status = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcliAndVerify(device, tapEnv,
						BroadBandWebPaConstants.WEBPA_PARAM_FOR_RFC_WIFI_BLASTER_ENABLE, BroadBandTestConstants.FALSE);
				if (status) {
					LOGGER.info("STEP " + stepNumber + ": ACTUAL : Wifi Blaster RFC parameter is disabled by deafult");
				} else {
					LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
				}
				LOGGER.info("**********************************************************************************");
				tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

				/**
				 * STEP 35 : Validate Wifi Blaster Report Active measurements parameter is
				 * disabled by default
				 */
				stepNum = "s" + (++stepNumber);
				status = false;
				LOGGER.info("**********************************************************************************");
				LOGGER.info("STEP " + stepNumber
						+ ": DESCRIPTION : Validate Wifi Blaster Report Active measurements parameter is disabled by default");
				LOGGER.info("STEP " + stepNumber
						+ ": ACTION : Execute webpa command: Device.WiFi.X_RDKCENTRAL-COM_Report.WifiClient.ActiveMeasurements.Enable");
				LOGGER.info("STEP " + stepNumber
						+ ": EXPECTED : Wifi Blaster Report Active measurements parameter must be disabled by default");
				LOGGER.info("**********************************************************************************");
				errorMessage = "Wifi Blaster Report Active measurements parameter is not disabled by default";
				status = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcliAndVerify(device, tapEnv,
						BroadBandWebPaConstants.WEBPA_PARAM_FOR_WIFI_BLASTER_TRIGGER, BroadBandTestConstants.FALSE);
				if (status) {
					LOGGER.info("STEP " + stepNumber
							+ ": ACTUAL : Wifi Blaster Report Active measurements parameter is disabled by default");
				} else {
					LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
				}
				LOGGER.info("**********************************************************************************");
				tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

				/**
				 * STEP 36 : Validate Wifi Blaster Packet Size parameter is 1470 by default
				 */
				stepNum = "s" + (++stepNumber);
				status = false;
				LOGGER.info("**********************************************************************************");
				LOGGER.info("STEP " + stepNumber
						+ ": DESCRIPTION : Validate Wifi Blaster Packet Size parameter is 1470 by default");
				LOGGER.info("STEP " + stepNumber
						+ ": ACTION : Execute webpa command: Device.WiFi.X_RDKCENTRAL-COM_Report.WifiClient.ActiveMeasurements.PacketSize");
				LOGGER.info("STEP " + stepNumber
						+ ": EXPECTED : Wifi Blaster Packet size parameter must be 1470 by default");
				LOGGER.info("**********************************************************************************");
				errorMessage = "Wifi Blaster Packet size parameter is not 1470 by default";
				status = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcliAndVerify(device, tapEnv,
						BroadBandWebPaConstants.WEBPA_PARAM_FOR_BLASTER_PACKETSIZE, BroadBandTestConstants.STRING_1470);
				if (status) {
					LOGGER.info(
							"STEP " + stepNumber + ": ACTUAL : Wifi Blaster Packet size parameter is 1470 by default");
				} else {
					LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
				}
				LOGGER.info("**********************************************************************************");
				tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

				/**
				 * STEP 37 : Validate Wifi Blaster SampleDuration parameter is 400 by default
				 */
				stepNum = "s" + (++stepNumber);
				status = false;
				LOGGER.info("**********************************************************************************");
				LOGGER.info("STEP " + stepNumber
						+ ": DESCRIPTION : Validate Wifi Blaster SampleDuration parameter is 400 by default");
				LOGGER.info("STEP " + stepNumber
						+ ": ACTION : Execute webpa command: Device.WiFi.X_RDKCENTRAL-COM_Report.WifiClient.ActiveMeasurements.SampleDuration");
				LOGGER.info("STEP " + stepNumber
						+ ": EXPECTED : Wifi Blaster Sample Duration parameter must be 1000 by default");
				LOGGER.info("**********************************************************************************");
				errorMessage = "Wifi Blaster Sample Duration parameter is not 400 by default";
				status = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcliAndVerify(device, tapEnv,
						BroadBandWebPaConstants.WEBPA_PARAM_FOR_BLASTER_SAMPLEDURATION,
						BroadBandTestConstants.STRING_400);
				if (status) {
					LOGGER.info("STEP " + stepNumber
							+ ": ACTUAL : Wifi Blaster Sample Duration parameter is 400 by default");
				} else {
					LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
				}
				LOGGER.info("**********************************************************************************");
				tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

				/**
				 * STEP 38 : Validate Wifi Blaster NumberOfSamples parameter is 5 by default
				 */
				stepNum = "s" + (++stepNumber);
				status = false;
				LOGGER.info("**********************************************************************************");
				LOGGER.info("STEP " + stepNumber
						+ ": DESCRIPTION : Validate Wifi Blaster NumberOfSamples parameter is 5 by default");
				LOGGER.info("STEP " + stepNumber
						+ ": ACTION : Execute webpa command: Device.WiFi.X_RDKCENTRAL-COM_Report.WifiClient.ActiveMeasurements.NumberOfSamples");
				LOGGER.info("STEP " + stepNumber
						+ ": EXPECTED : Wifi Blaster NumberOfSamples parameter must be 10 by default");
				LOGGER.info("**********************************************************************************");
				errorMessage = "Wifi Blaster NumberOfSamples parameter is not 5 by default";
				status = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcliAndVerify(device, tapEnv,
						BroadBandWebPaConstants.WEBPA_PARAM_FOR_BLASTER_NUMOFSAMPLES, BroadBandTestConstants.STRING_5);
				if (status) {
					LOGGER.info(
							"STEP " + stepNumber + ": ACTUAL : Wifi Blaster NumberOfSamples parameter is 5 by default");
				} else {
					LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
				}
				LOGGER.info("**********************************************************************************");
				tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

				/**
				 * STEP 39 : Validate Wifi Blaster PlanID parameter is Blank/Empty by default
				 */
				stepNum = "s" + (++stepNumber);
				status = false;
				LOGGER.info("**********************************************************************************");
				LOGGER.info("STEP " + stepNumber
						+ ": DESCRIPTION : Validate Wifi Blaster PlanID parameter is Null by default");
				LOGGER.info("STEP " + stepNumber
						+ ": ACTION : Execute webpa command: Device.WiFi.X_RDKCENTRAL-COM_Report.WifiClient.ActiveMeasurements.Plan.PlanID");
				LOGGER.info("STEP " + stepNumber
						+ ": EXPECTED : Wifi Blaster Plan ID parameter must be blank/empty by default");
				LOGGER.info("**********************************************************************************");
				errorMessage = "Wifi Blaster PlanID parameter is not blank/empty by default";

				try {
					response = tapEnv.executeWebPaCommand(device,
							BroadBandWebPaConstants.WEBPA_PARAM_FOR_BLASTER_PLANID);
				} catch (Exception e) {
					LOGGER.error("Exception caught while retrieving value using webpa: " + e.getMessage());
				}
				status = (response != null && response.length() == 0);
				if (status) {
					LOGGER.info("STEP " + stepNumber
							+ ": ACTUAL : Wifi Blaster PlanID parameter is blank/empty by default");
				} else {
					LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
				}
				LOGGER.info("**********************************************************************************");
				tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

				/**
				 * STEP 40 : Validate Wifi Blaster StepNumberOfEntries parameter is 32 by
				 * default
				 */
				stepNum = "s" + (++stepNumber);
				status = false;
				LOGGER.info("**********************************************************************************");
				LOGGER.info("STEP " + stepNumber
						+ ": DESCRIPTION : Validate Wifi Blaster StepNumberOfEntries parameter is 32 by default");
				LOGGER.info("STEP " + stepNumber
						+ ": ACTION : Execute webpa command: Device.WiFi.X_RDKCENTRAL-COM_Report.WifiClient.ActiveMeasurements.Plan.StepNumberOfEntries");
				LOGGER.info("STEP " + stepNumber
						+ ": EXPECTED : Wifi Blaster StepNumberOfEntries parameter must be 32 by default");
				LOGGER.info("**********************************************************************************");
				errorMessage = "Wifi Blaster StepNumberOfEntries parameter is not 32 by default";
				status = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcliAndVerify(device, tapEnv,
						BroadBandWebPaConstants.WEBPA_PARAM_FOR_BLASTER_NUMOFENTRIES,
						String.valueOf(BroadBandTestConstants.CONSTANT_32));
				if (status) {
					LOGGER.info("STEP " + stepNumber
							+ ": ACTUAL : Wifi Blaster StepNumberOfEntries parameter is 32 by default");
				} else {
					LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
				}
				LOGGER.info("**********************************************************************************");
				tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

				/**
				 * STEP 41 : Validate Wifi Blaster Step.{i}.StepID parameter is 0 by default for
				 * selected values of 1,9,16,31
				 */
				stepNum = "s" + (++stepNumber);
				status = false;
				LOGGER.info("**********************************************************************************");
				LOGGER.info("STEP " + stepNumber
						+ ": DESCRIPTION : Validate Wifi Blaster Step.{i}.StepID parameter is 0 by default for selected values of 1,9,16,31");
				LOGGER.info("STEP " + stepNumber
						+ ": ACTION : Execute webpa command: Execute webPA command:Device.WiFi.X_RDKCENTRAL-COM_Report.WifiClient.ActiveMeasurements.Plan.Step.{i}.StepID");
				LOGGER.info("STEP " + stepNumber
						+ ": EXPECTED : Wifi Blaster Step.{i}.StepID parameter must be 0 by default for selected values of 1,9,16,31");
				LOGGER.info("**********************************************************************************");
				errorMessage = "Wifi Blaster Step.{i}.StepID parameter is not 0 by default for selected values of 1,9,16,31";
				for (String value : BroadBandTestConstants.WIFI_BLASTER_STEPID_VALUES) {
					status = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcliAndVerify(device, tapEnv,
							BroadBandWebPaConstants.WEBPA_PARAM_FOR_BLASTER_STEPID
									.replace(BroadBandTestConstants.TR181_NODE_REF, value),
							BroadBandTestConstants.STRING_VALUE_ZERO);
					if (!status) {
						errorMessage = "Wifi Blaster Step.{" + value + "}.StepID parameter is not 0 by default";
						break;
					}
				}
				if (status) {
					LOGGER.info("STEP " + stepNumber
							+ ": ACTUAL : Wifi Blaster Step.{i}.StepID parameter is 0 by default for selected values of 1,9,16,31");
				} else {
					LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
				}
				LOGGER.info("**********************************************************************************");
				tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

				/**
				 * STEP 42 : Validate Wifi Blaster Step.{i}.SourceMac parameter is Blank/Empty
				 * by default for selected values of 1,9,16,31
				 */
				stepNum = "s" + (++stepNumber);
				status = false;
				LOGGER.info("**********************************************************************************");
				LOGGER.info("STEP " + stepNumber
						+ ": DESCRIPTION : Validate Wifi Blaster Step.{i}.SourceMac	 parameter is Null by default for selected values of 1,9,16,31");
				LOGGER.info("STEP " + stepNumber
						+ ": ACTION : Execute webpa command: Execute webPA command:Device.WiFi.X_RDKCENTRAL-COM_Report.WifiClient.ActiveMeasurements.Plan.Step.{i}.SourceMac");
				LOGGER.info("STEP " + stepNumber
						+ ": EXPECTED : Wifi Blaster Step.{i}.SourceMac parameter must be blank/empty by default for  selected values of 1,9,16,31");
				LOGGER.info("**********************************************************************************");
				errorMessage = "Wifi Blaster Step.{i}.SourceMac parameter is not blank/empty by default for  selected values of 1,9,16,31";

				for (String value : BroadBandTestConstants.WIFI_BLASTER_STEPID_VALUES) {
					try {
						response = tapEnv.executeWebPaCommand(device,
								BroadBandWebPaConstants.WEBPA_PARAM_FOR_BLASTER_SOURCEMAC
										.replace(BroadBandTestConstants.TR181_NODE_REF, value));
					} catch (Exception e) {
						LOGGER.error("Exception caught while retrieving value using webpa: " + e.getMessage());
					}
					status = !(response == null || response.length() != 0);
					if (!status) {
						errorMessage = "Wifi Blaster Step.{" + value
								+ "}.SourceMac parameter is not blank/empty by default";
						break;
					}
				}
				if (status) {
					LOGGER.info("STEP " + stepNumber
							+ ": ACTUAL : Wifi Blaster Step.{i}.SourceMac parameter is blank/empty by default for  selected values of 1,9,16,31");
				} else {
					LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
				}
				LOGGER.info("**********************************************************************************");
				tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

				/**
				 * STEP 43 : Validate Wifi Blaster Step.{i}.StepID parameter is Blank/Empty by
				 * default for selected values of 1,9,16,31
				 */
				stepNum = "s" + (++stepNumber);
				status = false;
				LOGGER.info("**********************************************************************************");
				LOGGER.info("STEP " + stepNumber
						+ ": DESCRIPTION : Validate Wifi Blaster Step.{i}.DestMac parameter is Null by default for selected values of 1,9,16,31");
				LOGGER.info("STEP " + stepNumber
						+ ": ACTION : Execute webpa command: Execute webPA command:Device.WiFi.X_RDKCENTRAL-COM_Report.WifiClient.ActiveMeasurements.Plan.Step.{i}.DestMac");
				LOGGER.info("STEP " + stepNumber
						+ ": EXPECTED : Wifi Blaster Step.{i}.DestMac parameter must be blank/empty by default for  selected values of 1,9,16,31");
				LOGGER.info("**********************************************************************************");
				errorMessage = "Wifi Blaster Step.{i}.DestMac parameter is not blank/empty by default for  selected values of 1,9,16,31";
				for (String value : BroadBandTestConstants.WIFI_BLASTER_STEPID_VALUES) {

					try {
						response = tapEnv.executeWebPaCommand(device,
								BroadBandWebPaConstants.WEBPA_PARAM_FOR_BLASTER_DESTMAC
										.replace(BroadBandTestConstants.TR181_NODE_REF, value));
					} catch (Exception e) {
						LOGGER.error("Exception caught while retrieving value using webpa: " + e.getMessage());
					}
					status = !(response == null || response.length() != 0);
					if (!status) {
						errorMessage = "Wifi Blaster Step.{" + value
								+ "}.DestMac parameter is not blank/empty by default";
						break;
					}
				}
				if (status) {
					LOGGER.info("STEP " + stepNumber
							+ ": ACTUAL : Wifi Blaster Step.{i}.DestMac parameter is blank/empty by default for  selected values of 1,9,16,31");
				} else {
					LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
				}
				LOGGER.info("**********************************************************************************");
				tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);
			} else {
				for (int num = BroadBandTestConstants.CONSTANT_34; num <= BroadBandTestConstants.CONSTANT_38
						+ BroadBandTestConstants.CONSTANT_5; num++) {
					stepNum = "s" + num;
					tapEnv.updateExecutionForAllStatus(device, testCaseId, stepNum, ExecutionStatus.NOT_APPLICABLE,
							"step is applicable for specific Platforms only", false);
				}
			}

			/**
			 * STEP 44 : Verify force wifi disable value set to default after factory reset
			 */
			stepNumber = BroadBandTestConstants.CONSTANT_38 + BroadBandTestConstants.CONSTANT_5;
			stepNum = "s" + (++stepNumber);
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP " + stepNumber
					+ ": DESCRIPTION :Verify force wifi disable value set to default after factory reset");
			LOGGER.info("STEP " + stepNumber
					+ ": ACTION : Execute webpa command: Execute webPA command:Device.WiFi.X_RDK-CENTRAL_COM_ForceDisable");
			LOGGER.info("STEP " + stepNumber + ": EXPECTED : paramter should respond value as false");
			LOGGER.info("**********************************************************************************");
			errorMessage = "force disable value is not false after factory reset";
			status = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcliAndVerify(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_FORCE_WIFI_DISABLE, BroadBandTestConstants.FALSE);
			if (status) {
				LOGGER.info("STEP " + stepNumber + ": ACTUAL : force disable value is false after factory reset");
			} else {
				LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			isSupportedDevices = null;
			isSupportedDevices = BroadbandPropertyFileHandler.isDeviceSupported(device);

			if (isSupportedDevices || DeviceModeHandler.isBusinessClassDevice(device)) {

				/**
				 * STEP 45 : Verify default value of selfheal aggressive Interval is 5 min using
				 * webpa
				 */
				stepNum = "s" + (++stepNumber);
				status = false;
				LOGGER.info("**********************************************************************************");
				LOGGER.info("STEP " + stepNumber
						+ ": DESCRIPTION : Verify default value of selfheal aggressive Interval is 5 min using webpa");
				LOGGER.info("STEP " + stepNumber
						+ ": ACTION : Execute webpa command: DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.SoftwareProcessManager.SelfHeal.AggressiveInterval");
				LOGGER.info("STEP " + stepNumber
						+ ": EXPECTED : Webpa get operation should be successful and response should be 5");
				LOGGER.info("**********************************************************************************");
				errorMessage = "Failed to verify default value of selfheal aggressive interval as 5 min using webpa";
				status = BroadBandWebPaUtils.getAndVerifyWebpaValueInPolledDuration(device, tapEnv,
						BroadBandWebPaConstants.WEBPA_PARAM_AGGRESSIVE_SELFHEAL_INTERVAL,
						BroadBandTestConstants.STRING_5, BroadBandTestConstants.ONE_MINUTE_IN_MILLIS,
						BroadBandTestConstants.TWENTY_SECOND_IN_MILLIS);
				if (status) {
					LOGGER.info("STEP " + stepNumber
							+ ": ACTUAL : Successfully verified selfheal aggressive interval as 5 min using webpa");
				} else {
					LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
				}
				LOGGER.info("**********************************************************************************");
				tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

				/**
				 * STEP 46 : Verify default value of non-critical selfheal interval is 15 min
				 * using webpa
				 */
				stepNum = "s" + (++stepNumber);
				status = false;
				LOGGER.info("**********************************************************************************");
				LOGGER.info("STEP " + stepNumber
						+ ": DESCRIPTION : Verify default value of non-critical selfheal interval is 15 min using webpa");
				LOGGER.info("STEP " + stepNumber
						+ ": ACTION : Execute webpa command: Device.SelfHeal.ResourceMonitor.X_RDKCENTRAL-COM_UsageComputeWindow");
				LOGGER.info("STEP " + stepNumber
						+ ": EXPECTED : Webpa get operation should be successful and response should be 15");
				LOGGER.info("**********************************************************************************");
				errorMessage = "Failed to verify default value of base selfheal interval as 15 min";
				status = BroadBandWebPaUtils.getAndVerifyWebpaValueInPolledDuration(device, tapEnv,
						BroadBandWebPaConstants.USAGE_COMPUTE_WINDOW, BroadBandTestConstants.STRING_CONSTANT_15,
						BroadBandTestConstants.ONE_MINUTE_IN_MILLIS, BroadBandTestConstants.TWENTY_SECOND_IN_MILLIS);
				if (status) {
					LOGGER.info("STEP " + stepNumber
							+ ": ACTUAL : Successfully verified base selfheal interval as 15 min using webpa");
				} else {
					LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
				}
				LOGGER.info("**********************************************************************************");
				tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);
			} else {
				tapEnv.updateExecutionForAllStatus(device, testCaseId, "s" + (++stepNumber),
						ExecutionStatus.NOT_APPLICABLE, "step is applicable for specific Platforms only", false);
				tapEnv.updateExecutionForAllStatus(device, testCaseId, "s" + (++stepNumber),
						ExecutionStatus.NOT_APPLICABLE, "step is applicable for specific Platforms only", false);
			}
			isSupportedDevices = null;

			isSupportedDevices = BroadbandPropertyFileHandler.isDeviceSupported(device);

			if (isSupportedDevices || CommonMethods.isAtomSyncAvailable(device, tapEnv)
					|| DeviceModeHandler.isBusinessClassDevice(device) || DeviceModeHandler.isFibreDevice(device)) {

				/**
				 * STEP 47 : Verify RDKFirmwareUpgrader tr181 is disabled by default
				 */
				stepNum = "s" + (++stepNumber);
				status = false;
				LOGGER.info("**********************************************************************************");
				LOGGER.info("STEP " + stepNumber
						+ ": DESCRIPTION : Verify RDKFirmwareUpgrader tr181 is disabled by default");
				LOGGER.info("STEP " + stepNumber
						+ ": ACTION : Execute webpa get command: tr181.Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.RDKFirmwareUpgrader.Enable");
				LOGGER.info("STEP " + stepNumber
						+ ": EXPECTED : tr181.Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.RDKFirmwareUpgrader.Enable should respond false value");
				LOGGER.info("**********************************************************************************");
				errorMessage = BroadBandWebPaConstants.WEBPA_PARAM_RDK_FIRMWARE_UPGRADER
						+ " RDK Firmware upgrader RFC parameter is not disabled by default";
				status = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcliAndVerify(device, tapEnv,
						BroadBandWebPaConstants.WEBPA_PARAM_RDK_FIRMWARE_UPGRADER, BroadBandTestConstants.FALSE);
				if (status) {
					LOGGER.info("STEP " + stepNumber
							+ ": ACTUAL : RDK Firmware upgrader RFC parameter is disabled by default");
				} else {
					LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
				}
				LOGGER.info("**********************************************************************************");
				tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

				/**
				 * STEP 48 : Enable RDKFirmwareUpgrader tr181 using webpa
				 */
				stepNum = "s" + (++stepNumber);
				status = false;
				LOGGER.info("**********************************************************************************");
				LOGGER.info("STEP " + stepNumber + ": DESCRIPTION : Enable RDKFirmwareUpgrader tr181 using webpa");
				LOGGER.info("STEP " + stepNumber
						+ ": ACTION : Execute webpa command: Execute webpa set command: tr181.Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.RDKFirmwareUpgrader.Enable to true");
				LOGGER.info(
						"STEP " + stepNumber + ": EXPECTED : 200 success response should get for webpa set request");
				LOGGER.info("**********************************************************************************");
				errorMessage = "Not able to enable tr181.Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.RDKFirmwareUpgrader.Enable using webpa";
				firmwareUpgradeEnable = BroadBandWebPaUtils.setAndVerifyParameterValuesUsingWebPaorDmcli(device, tapEnv,
						BroadBandWebPaConstants.WEBPA_PARAM_RDK_FIRMWARE_UPGRADER, BroadBandTestConstants.CONSTANT_3,
						BroadBandTestConstants.TRUE);
				status = firmwareUpgradeEnable;
				if (status) {
					LOGGER.info(
							"STEP " + stepNumber + ": ACTUAL : Enabled RDK Firware upgrader parameter using webpa.");
				} else {
					LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
				}
				LOGGER.info("**********************************************************************************");
				tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

				/**
				 * STEP 49 : Verify new firmware schedule script is present under /etc
				 */
				stepNum = "s" + (++stepNumber);
				status = false;
				LOGGER.info("**********************************************************************************");
				LOGGER.info("STEP " + stepNumber
						+ ": DESCRIPTION : Verify new firmware schedule script is present under /etc");
				LOGGER.info(
						"STEP " + stepNumber + ": ACTION : if /etc/firmwareSched.sh; then echo true;else echo false;");
				LOGGER.info("STEP " + stepNumber + ": EXPECTED : script file should be present under /etc");
				LOGGER.info("**********************************************************************************");
				errorMessage = "firmwareSched.sh is not present in /etc";
				status = CommonMethods.isFileExists(device, tapEnv,
						BroadBandCommandConstants.FILE_ETC_FIRMWARE_SCHED_SH);
				if (status) {
					LOGGER.info("STEP " + stepNumber + ": ACTUAL : firmwareSched.sh is present under /etc");
				} else {
					LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
				}
				LOGGER.info("**********************************************************************************");
				tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);
			} else {
				for (int num = BroadBandTestConstants.CONSTANT_45
						+ BroadBandTestConstants.CONSTANT_2; num <= BroadBandTestConstants.CONSTANT_45
								+ BroadBandTestConstants.CONSTANT_4; num++) {
					stepNum = "s" + num;
					tapEnv.updateExecutionForAllStatus(device, testCaseId, stepNum, ExecutionStatus.NOT_APPLICABLE,
							"step is applicable for specific Platforms only", false);
				}
			}

		} catch (Exception e) {
			errorMessage = errorMessage + e.getMessage();
			LOGGER.error(errorMessage);
			CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNum, status, errorMessage,
					false);
		} finally {
			LOGGER.info("################### STARTING POST-CONFIGURATIONS ###################");
			LOGGER.info("POST-CONDITION STEPS");
			if (firmwareUpgradeEnable) {
				status = false;
				LOGGER.info("POST-CONDITION - 1 : DESCRIPTION : Disable RDKFirmwareUpgrader tr181 using webpa");
				LOGGER.info(
						"POST-CONDITION - 1 : ACTION : Execute webpa set command: tr181.Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.RDKFirmwareUpgrader.Enable to false");
				LOGGER.info("POST-CONDITION - 1 : EXPECTED : 200 success response should get for webpa set request");
				status = BroadBandWebPaUtils.setAndVerifyParameterValuesUsingWebPaorDmcli(device, tapEnv,
						BroadBandWebPaConstants.WEBPA_PARAM_RDK_FIRMWARE_UPGRADER, BroadBandTestConstants.CONSTANT_3,
						BroadBandTestConstants.FALSE);
				if (status) {
					LOGGER.info("POST-CONDITION - 1 :Disabled RDKFirmwareUpgrader tr181 using webpa");
				} else {
					LOGGER.info("POST-CONDITION - 1 : Failed to disable RDKFirmwareUpgrader tr181 using webpa");
				}
			} else {
				LOGGER.info("POST-CONDITION - 1 is NA since RDKFirmwareUpgrader tr181 parameter value is not changed");
			}
			LOGGER.info("POST-CONDITION - 2 : DESCRIPTION : BEGIN BROAD BAND DEVICE REACTIVATION");
			LOGGER.info("POST-CONDITION - 2 : ACTION : BROAD BAND DEVICE REACTIVATION");
			LOGGER.info("POST-CONDITION - 2 : EXPECTED : device should get reactivated");
			LOGGER.info("### POST-CONDITION - 2 ### BEGIN BROAD BAND DEVICE REACTIVATION");
			BroadBandWiFiUtils.reactivateDeviceUsingWebpaOrSnmp(tapEnv, device);

			LOGGER.info("################### COMPLETED POST-CONFIGURATIONS ###################");
		}
		LOGGER.info("ENDING TEST CASE: TC-RDKB-FACTORYRESET_REBOOT-1001");

	}

    /**
     * Test to verify the behavior of SNMP MIB DOCS-CABLE-DEVICE-MIB::docsDevResetNow(.1.3.6.1.2.1.69.1.1.3) using eCM
     * IP Address. Device should not reboot if we set docsDevResetNow as 2 using SNMP command. If we set docsDevResetNow
     * as 1, then device should reboot and comes up within 5 minutes.
     * 
     * <ol>
     * <li>Step 1 : Verify Setting SNMP MIB DOCS-CABLE-DEVICE-MIB::docsDevResetNow(.1.3.6.1.2.1.69.1.1.3) to 2.</li>
     * <li>Step 2 : Verify device is not rebooting after setting SNMP MIB
     * DOCS-CABLE-DEVICE-MIB::docsDevResetNow(.1.3.6.1.2.1.69.1.1.3) to 2.</li>
     * <li>Step 3 : Verify Setting SNMP MIB DOCS-CABLE-DEVICE-MIB::docsDevResetNow(.1.3.6.1.2.1.69.1.1.3) to 1.</li>
     * <li>Step 4 : Verify device is going for reboot after setting SNMP MIB
     * DOCS-CABLE-DEVICE-MIB::docsDevResetNow(.1.3.6.1.2.1.69.1.1.3) to 1.</li>
     * <li>Step 5 : Verify device comes up after successful reboot.</li>
     * <li>Step 6 : Verify RDKB_REBOOT:docsDevResetNow log in Consolelog.txt.0.</li>
     * </ol>
     * 
     * @param settop
     *            Settop to be used
     * @author Prabhakaran
     * @refactor anandam
     */
    @Test(enabled = true, dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, groups = TestGroup.SYSTEM)
    @TestDetails(testUID = "TC-RDKB-REBOOT-5001")
    public void verifyDeviceRebootUsingDocsDevResetNowSnmpCommand(Dut settop) {
	// Variable declaration starts
	boolean status = false;
	String testCaseId = "TC-RDKB-REBOOT-501";
	String stepNumber = "s1";
	String errorMessage = null;
	String snmpSetOutput = null;
	// Variable declaration ends

	LOGGER.info("#######################################################################################");
	LOGGER.info("STARTING TEST CASE: TC-RDKB-REBOOT-5001");
	LOGGER.info(
		"TEST DESCRIPTION: Verify the docsDevResetNow using DOCS-CABLE-DEVICE-MIB::docsDevResetNow(.1.3.6.1.2.1.69.1.1.3)");

	LOGGER.info("TEST STEPS : ");
	LOGGER.info("1. Verify Setting SNMP MIB DOCS-CABLE-DEVICE-MIB::docsDevResetNow(.1.3.6.1.2.1.69.1.1.3) to 2.");
	LOGGER.info(
		"2. Verify device is not rebooting after setting SNMP MIB DOCS-CABLE-DEVICE-MIB::docsDevResetNow(.1.3.6.1.2.1.69.1.1.3) to 2.");
	LOGGER.info("3. Verify Setting SNMP MIB DOCS-CABLE-DEVICE-MIB::docsDevResetNow(.1.3.6.1.2.1.69.1.1.3) to 1.");
	LOGGER.info(
		"4. Verify device is going for reboot after setting SNMP MIB DOCS-CABLE-DEVICE-MIB::docsDevResetNow(.1.3.6.1.2.1.69.1.1.3) to 1.");
	LOGGER.info("5. Verify device comes up after successful reboot.");
	LOGGER.info("6. Verify RDKB_REBOOT:docsDevResetNow log in Consolelog.txt.0.");
	LOGGER.info("#######################################################################################");

	try {
	    stepNumber = "S1";
	    errorMessage = "Unable to set SNMP MIB DOCS-CABLE-DEVICE-MIB::docsDevResetNow(.1.3.6.1.2.1.69.1.1.3) value as 2.";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 1: DESCRIPTION : Verify Setting SNMP MIB DOCS-CABLE-DEVICE-MIB::docsDevResetNow(.1.3.6.1.2.1.69.1.1.3) to 2.");
	    LOGGER.info("STEP 1: ACTION : Execute SNMP Set command for MIB: .1.3.6.1.2.1.69.1.1.3 and set value as 2.");
	    LOGGER.info("STEP 1: EXPECTED : SNMP Set command should execute successfully and return output as 2.");
	    LOGGER.info("**********************************************************************************");
	    snmpSetOutput = BroadBandSnmpUtils.snmpSetOnEcm(tapEnv, settop,
		    BroadBandSnmpMib.ECM_RESET_MIB.getOid() + ".0", SnmpDataType.INTEGER,
		    BroadBandTestConstants.STRING_VALUE_TWO);
	    status = BroadBandCommonUtils.compareValues(BroadBandTestConstants.CONSTANT_TXT_COMPARISON,
		    BroadBandTestConstants.STRING_VALUE_TWO, snmpSetOutput);
	    if (status) {
		LOGGER.info(
			"STEP 1: ACTUAL: SNMP MIB DOCS-CABLE-DEVICE-MIB::docsDevResetNow(.1.3.6.1.2.1.69.1.1.3) is set to value 2.");
	    } else {
		LOGGER.error("STEP 1: ACTUAL: " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(settop, testCaseId, stepNumber, status, errorMessage, true);

	    stepNumber = "S2";
	    errorMessage = "Device went for reboot even after setting SNMP MIB DOCS-CABLE-DEVICE-MIB::docsDevResetNow(.1.3.6.1.2.1.69.1.1.3) to 2.";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 2: DESCRIPTION : Verify device is not rebooting after setting SNMP MIB DOCS-CABLE-DEVICE-MIB::docsDevResetNow(.1.3.6.1.2.1.69.1.1.3) to 2.");
	    LOGGER.info("STEP 2: ACTION : Check device accessibility after every 30 seconds continously for 5 min.");
	    LOGGER.info("STEP 2: EXPECTED : Device shouldn't go for reboot.");
	    LOGGER.info("**********************************************************************************");
	    status = !CommonMethods.isSTBRebooted(tapEnv, settop, BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS,
		    BroadBandTestConstants.CONSTANT_10);
	    if (status) {
		LOGGER.info(
			"STEP 2: ACTUAL: Device didn't go for reboot after setting SNMP MIB DOCS-CABLE-DEVICE-MIB::docsDevResetNow(.1.3.6.1.2.1.69.1.1.3) to 2.");
	    } else {
		LOGGER.error("STEP 2: ACTUAL: " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(settop, testCaseId, stepNumber, status, errorMessage, true);

	    /*
	     * Due to large buffer size, it takes more time for searching in buffer. So clearing buffer and start
	     * buffering the trace.
	     */
	    tapEnv.cleanupTraceBuffer(settop);

	    stepNumber = "S3";
	    errorMessage = "Unable to set SNMP MIB DOCS-CABLE-DEVICE-MIB::docsDevResetNow(.1.3.6.1.2.1.69.1.1.3) value as 1.";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 3: DESCRIPTION : Verify Setting SNMP MIB DOCS-CABLE-DEVICE-MIB::docsDevResetNow(.1.3.6.1.2.1.69.1.1.3) to 1.");
	    LOGGER.info("STEP 3: ACTION : Execute SNMP Set command for MIB: .1.3.6.1.2.1.69.1.1.3 and set value as 1.");
	    LOGGER.info("STEP 3: EXPECTED : SNMP Set command should execute successfully and return output as 1.");
	    LOGGER.info("**********************************************************************************");
	    tapEnv.executeCommandUsingSsh(settop, BroadBandCommandConstants.COMMAND_TO_COPY_TO_NVRAM_CONSOLELOG);
	    snmpSetOutput = BroadBandSnmpUtils.snmpSetOnEcm(tapEnv, settop,
		    BroadBandSnmpMib.ECM_RESET_MIB.getOid() + ".0", SnmpDataType.INTEGER,
		    BroadBandTestConstants.STRING_VALUE_ONE);
	    status = BroadBandCommonUtils.compareValues(BroadBandTestConstants.CONSTANT_TXT_COMPARISON,
		    BroadBandTestConstants.STRING_VALUE_ONE, snmpSetOutput);
	    boolean isSTBRebooted = CommonMethods.isSTBRebooted(tapEnv, settop,
		    BroadBandTestConstants.TEN_SECOND_IN_MILLIS, BroadBandTestConstants.CONSTANT_60);

	    if (status) {
		LOGGER.info(
			"STEP 3: ACTUAL: SNMP MIB DOCS-CABLE-DEVICE-MIB::docsDevResetNow(.1.3.6.1.2.1.69.1.1.3) is set to value 1.");
	    } else {
		LOGGER.error("STEP 3: ACTUAL: " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(settop, testCaseId, stepNumber, status, errorMessage, true);

	    stepNumber = "S4";
	    errorMessage = "Device did not go for reboot even after setting SNMP MIB DOCS-CABLE-DEVICE-MIB::docsDevResetNow(.1.3.6.1.2.1.69.1.1.3) to 1.";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 4: DESCRIPTION : Verify device is going for reboot and comes up later.");
	    LOGGER.info(
		    "STEP 4: ACTION : Check device went for reboot after every 30 seconds continously for 5 min and and once go for reboot, wait for device to come up.");
	    LOGGER.info("STEP 4: EXPECTED : Device should go for reboot and should come up with all processes up.");
	    LOGGER.info("**********************************************************************************");
	    if (isSTBRebooted) {
		LOGGER.info("Device rebooted successfully.");
		errorMessage = "Device is not coming up after successful reboot.";
		status = CommonMethods.waitForEstbIpAcquisition(tapEnv, settop);
	    }
	    if (status) {
		LOGGER.info("STEP 4: ACTUAL: Device went for reboot and came up with all the processes.");
	    } else {
		LOGGER.error("STEP 4: ACTUAL: " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(settop, testCaseId, stepNumber, status, errorMessage, true);

	    stepNumber = "S5";
	    errorMessage = "Unable to verify last reboot reason.";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 5: DESCRIPTION : Verify last reboot reason.");
	    LOGGER.info(
		    "STEP 5: ACTION : Execute the WebPa Get command for following param: Device.DeviceInfo.X_RDKCENTRAL-COM_LastRebootReason.");
	    LOGGER.info("STEP 5: EXPECTED : Last reboot reason should be 'snmp-reboot'.");
	    LOGGER.info("**********************************************************************************");
	    status = BroadBandCommonUtils.verifySnmpRebootReason(settop, tapEnv);
	    if (status) {
		LOGGER.info("STEP 5: ACTUAL: Last reboot reason is verified successfully as 'snmp-reboot'.");
	    } else {
		LOGGER.error("STEP 5: ACTUAL: " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(settop, testCaseId, stepNumber, status, errorMessage, false);

	    stepNumber = "S6";
	    errorMessage = "Log message is not found after Rebooting the device thorugh docsDevResetNow.0 mib Since this logging might or might not appear,"
		    + " marking the step as NA";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 6: DESCRIPTION : Verify RDKB_REBOOT log message after setting docsDevResetNow.0 mib in Consolelog.txt.0.");
	    LOGGER.info("STEP 6: ACTION : SSH the device and look for required message in device logs.");
	    LOGGER.info(
		    "STEP 6: EXPECTED :Required log message:'RDKB_REBOOT:docsDevResetNow'for device, 'RDKB_REBOOT: Docsis_SNMP_Reboot request received, rebooting device' in /rdklogs/logs/SecConsole.txt.0 /nvram2/logs/SecConsole_lastreboot.txt.0 for arm devices and 'RDKB_REBOOT: SNMP Reboot request received, rebooting device' for fiber devices and 'RDKB_REBOOT: Reboot triggered by SNMP' for other devices should be present in Consolelog.txt.0/nvram2/logs/Consolelog.txt.0.");
	    LOGGER.info("**********************************************************************************");
	    status = BroadBandCommonUtils.verifyTelemetryMarkerForDeviceRebootInitiatedBySnmpDocDevMib(settop, tapEnv);
	    if (status) {
		LOGGER.info("STEP 6: ACTUAL: Required log message is present in Device Logs.");
		LOGGER.info("**********************************************************************************");
		tapEnv.updateExecutionStatus(settop, testCaseId, stepNumber, status, errorMessage, true);
	    } else {
		errorMessage = "Logging didn't appear.Marking the step as NA";
		LOGGER.error("STEP 6: ACTUAL: " + errorMessage);
		LOGGER.info("**********************************************************************************");
		tapEnv.updateExecutionForAllStatus(settop, testCaseId, stepNumber, ExecutionStatus.NOT_APPLICABLE,
			errorMessage, false);
	    }
	} catch (Exception exception) {
	    errorMessage = errorMessage + exception.getMessage();
	    LOGGER.error(errorMessage);
	    CommonUtils.updateTestStatusDuringException(tapEnv, settop, testCaseId, stepNumber, status, errorMessage,
		    true);
	} finally {
	    LOGGER.info("################### STARTING POST-CONFIGURATIONS ###################");
	    LOGGER.info("POST-CONDITION STEPS");
	    LOGGER.info("####################################################################");
	    LOGGER.info("POST-CONDITION 1 : DESCRIPTION : Remove tmp files");
	    LOGGER.info("POST-CONDITION 1 : ACTION : Remove /tmp/Consolelog.txt");
	    LOGGER.info("POST-CONDITION 1 : EXPECTED : Files removed successfully");
	    LOGGER.info("####################################################################");
	    status = false;
	    errorMessage = "Failed to remove /tmp/Consolelog.txt file";
	    status = CommonUtils.removeFileandVerifyFileRemoval(tapEnv, settop,
		    BroadBandCommandConstants.FILE_PATH_TMP_CONSOLE_LOG);
	    if (status) {
		LOGGER.info("POST-CONDITION 1 : ACTUAL : File removed successfully");
	    } else {
		LOGGER.error("POST-CONDITION 1 : ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("POST-CONFIGURATIONS : FINAL STATUS - " + status);
	    LOGGER.info("################### COMPLETED POST-CONFIGURATIONS ###################");
	}
	LOGGER.info("ENDING TEST CASE: TC-RDKB-REBOOT-5001");
    }

    /**
     * Verify interface reboot due to brlan0 and verify logs in SelfHeal.txt.0 file
     * <ol>
     * <li>PRE CONDITION 1: Verify Selfheal is enabled in device.</li>
     * <li>PRE CONDITION 2: Verify whether Selfheal process is Up and Running in the Device.</li>
     * <li>Verify whether brlan0 is assigned properly with valid DHCPv4 address.</li>
     * <li>Verify whether brlan0 is assigned properly with valid DHCPv6 address.</li>
     * <li>Verify whether brlan0 interface is up.</li>
     * <li>Verify interface brlan0 status is brought to down.</li>
     * <li>Verify interface brlan0 interface comes up through Selfheal.</li>
     * <li>Verify required logs in SelfHeal.txt.0 file.</li>
     * <li>Verify whether brlan0 is assigned properly with valid DHCPv4 address.</li>
     * <li>Verify whether brlan0 is assigned properly with valid DHCPv6 address.</li>
     * </ol>
     * 
     * @param device{@link
     *            Dut}
     * 
     * @author prashant.mishra12
     * @refactor Said Hisham
     */
    @Test(enabled = true, dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, groups = TestGroup.SYSTEM)
    @TestDetails(testUID = "TC-RDKB-REBOOT-5003")
    public void testToverifyBrlan0interfaceReboot(Dut device) {
	// Variable Declaration begins
	String testCaseId = "TC-RDKB-REBOOT-503";
	String stepNum = "";
	String errorMessage = "";
	String response = "";
	String interfaceBrlan0Status = "";
	boolean status = false;
	int count = 0;
	// Variable Declaration Ends

	LOGGER.info("#######################################################################################");
	LOGGER.info("STARTING TEST CASE: TC-RDKB-REBOOT-5003");
	LOGGER.info("TEST DESCRIPTION: Verify interface reboot due to brlan0 and verify logs in SelfHeal.txt.0 file");

	LOGGER.info("TEST STEPS : ");
	LOGGER.info("PRE CONDITION 1. Verify selfheal is enabled in device.");
	LOGGER.info("PRE CONDITION 2. Verify whether Selfheal process is Up and Running in the Device.");
	LOGGER.info("1. Verify whether brlan0 is assigned properly with valid DHCPv4 address.");
	LOGGER.info("2. Verify whether brlan0 is assigned properly with valid DHCPv6 address.");
	LOGGER.info("3. Verify whether brlan0 interface is up.");
	LOGGER.info("4. Verify interface brlan0 status is brought to down.");
	LOGGER.info("5. Verify interface brlan0 interface comes up through selfheal.");
	LOGGER.info("6. Verify required logs in SelfHeal.txt.0 file.");
	LOGGER.info("7. Verify whether brlan0 is assigned properly with valid DHCPv4 address.");
	LOGGER.info("8. Verify whether brlan0 is assigned properly with valid DHCPv6 address.");
	LOGGER.info("#######################################################################################");

	try {
	    LOGGER.info("################### STARTING PRE-CONFIGURATIONS ###################");
	    LOGGER.info("PRE-CONDITION STEPS");

	    errorMessage = "Webpa is not Up and  not Running.";
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("PRE-CONDITION 1 : DESCRIPTION : Verify whether WebPA is Up and Running in the Device.");
	    LOGGER.info(
		    "PRE-CONDITION 1 : ACTION : Verifying Successful webpa Get response ,in case of failure rechecking for 8 minutes.");
	    LOGGER.info("PRE-CONDITION 1 : EXPECTED : WebPA should be Up and Running in the Device.");
	    LOGGER.info("**********************************************************************************");
	    try {
		status = BroadBandWebPaUtils.verifyWebPaProcessIsUp(tapEnv, device, true);
	    } catch (TestException exception) {
		errorMessage = exception.getMessage();
		LOGGER.error(errorMessage);
	    }
	    if (status) {
		LOGGER.info("PRE-CONDITION 1 : ACTUAL : WebPA is Up and Running in Device.");
	    } else {
		throw new TestException(
			BroadBandTestConstants.PRE_CONDITION_ERROR + "PRE-CONDITION 1 : FAILED : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");

	    errorMessage = "Failed to enable selfheal in device via webpa.";
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("PRE-CONDITION 2 : DESCRIPTION : Verify selfheal is enabled in device.");
	    LOGGER.info(
		    "PRE-CONDITION 2 : ACTION : Execute the following webpa parameters: Device.SelfHeal.X_RDKCENTRAL-COM_Enable");
	    LOGGER.info("PRE-CONDITION 2 : EXPECTED : Selfheal should be enabled successfully.");
	    LOGGER.info("**********************************************************************************");
	    try {
		status = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
			BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_SELFHEAL_PROCESS_ENABLE_STATUS,
			BroadBandTestConstants.CONSTANT_3, BroadBandTestConstants.TRUE);
	    } catch (Exception exception) {
		errorMessage = exception.getMessage();
		LOGGER.error(errorMessage);
	    }
	    if (status) {
		LOGGER.info("PRE-CONDITION 2 : ACTUAL : Selfheal enabled successfully via webpa.");
	    } else {
		throw new TestException(
			BroadBandTestConstants.PRE_CONDITION_ERROR + "PRE-CONDITION 2 : FAILED : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");

	    errorMessage = "Selfheal process is not running in device.";
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("PRE-CONDITION 3 : DESCRIPTION : Verify selfheal process is up and running.");
	    LOGGER.info(
		    "PRE-CONDITION 3 : ACTION : SSH the device and execute the following command: ps | grep -i ccsp and verify for < {self_heal_conne} /bin/sh /usr/ccsp/tad/self_heal_connectivity_test.sh > file.");
	    LOGGER.info("PRE-CONDITION 3 : EXPECTED : Selfheal process should be up and running.");
	    LOGGER.info("**********************************************************************************");
	    try {
		response = tapEnv.executeCommandUsingSsh(device, BroadBandCommandConstants.CMD_PS_GREP_CCSP);
		status = CommonMethods.isNotNull(response)
			&& response.contains(BroadBandTestConstants.SELFHEAL_PROCESS_UP_LOG);
	    } catch (Exception exception) {
		errorMessage = exception.getMessage();
		LOGGER.error(errorMessage);
	    }
	    if (status) {
		LOGGER.info("PRE-CONDITION 3 : ACTUAL : Selfheal process is Up and Running in Device.");
	    } else {
		throw new TestException(
			BroadBandTestConstants.PRE_CONDITION_ERROR + "PRE-CONDITION 3 : FAILED : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");

	    stepNum = "S1";
	    errorMessage = "Interface brlan0 is not up.";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 1: DESCRIPTION : Verify whether brlan0 interface is up.");
	    LOGGER.info(
		    "STEP 1: ACTION : SSH the device and Execute the following command:<ip a show brlan0> and verify interface status.");
	    LOGGER.info("STEP 1: EXPECTED : Interface brlan0 should be ruuning up.");
	    LOGGER.info("**********************************************************************************");
	    long startTime = System.currentTimeMillis();
	    do {
		interfaceBrlan0Status = BootTimeUtils.verifyInterfaceBrlan0UpStatus(device, tapEnv);
		status = CommonMethods.isNotNull(interfaceBrlan0Status) && interfaceBrlan0Status
			.equalsIgnoreCase(BroadBandConnectedClientTestConstants.RADIO_STATUS_UP);
	    } while (!status
		    && (System.currentTimeMillis() - startTime < BroadBandTestConstants.FIFTEEN_MINUTES_IN_MILLIS)
		    && BroadBandCommonUtils.hasWaitForDuration(tapEnv, BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS));
	    if (status) {
		LOGGER.info("STEP 1: ACTUAL : Interface brlan0 is verified succesfully and running up.");
	    } else {
		LOGGER.error("STEP 1: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

	    stepNum = "S2";
	    errorMessage = "Interface brlan0 is not assigned with valid DHCPv4 address.";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 2: DESCRIPTION : Verify whether brlan0 is assigned properly with valid DHCPv4 address.");
	    LOGGER.info(
		    "STEP 2: ACTION : SSH the device and Execute the following command:<ifconfig brlan0> and verify Ipv4 address.");
	    LOGGER.info("STEP 2: EXPECTED : Interface brlan0 should be assigned with valid DHCPv4 address.");
	    LOGGER.info("**********************************************************************************");
	    response = tapEnv.executeCommandUsingSsh(device, BroadBandTestConstants.IFCONFIG_BRLAN);
	    status = CommonMethods.patternMatcher(response, BroadBandTestConstants.INET_V4_ADDRESS_PATTERN);
	    if (status) {
		LOGGER.info("STEP 2: ACTUAL : Interface brlan0 is assigned with valid DHCPv4 address.");
	    } else {
		LOGGER.error("STEP 2: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

	    stepNum = "S3";
	    errorMessage = "Interface brlan0 is not assigned with valid DHCPv6 address.";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 3: DESCRIPTION : Verify whether brlan0 is assigned properly with valid DHCPv6 address.");
	    LOGGER.info(
		    "STEP 3: ACTION : SSH the device and Execute the following command:<ifconfig brlan0> and verify Ipv6 address.");
	    LOGGER.info("STEP 3: EXPECTED : Interface brlan0 should be assigned with valid DHCPv6 address.");
	    LOGGER.info("**********************************************************************************");
	    status = CommonMethods.patternMatcher(response, BroadBandTestConstants.INET_V6_ADDRESS_PATTERN);
	    if (status) {
		LOGGER.info("STEP 3: ACTUAL : Interface brlan0 is assigned with valid DHCPv6 address.");
	    } else {
		LOGGER.error("STEP 3: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

	    stepNum = "S4";
	    errorMessage = "Unable to bring brlan0 interface status to down.";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 4: DESCRIPTION : Verify interface brlan0 status is brought to down.");
	    LOGGER.info(
		    "STEP 4: ACTION : SSH the device and Execute the following command:<ifconfig brlan0 down> and verify status.");
	    LOGGER.info("STEP 4: EXPECTED : Interface brlan0 status should come to down status.");
	    LOGGER.info("**********************************************************************************");
	    /**
	     * Tailing the logs continuously from /rdklogs/logs/SelfHeal.txt.0 log file to /nvram/sample.txt for
	     * AtomConsole devices
	     */
	    /**
	     * Tailing the logs continuously from /rdklogs/logs/SelfHealAggressive.txt.0 log file to /nvram/sample.txt
	     * for other than AtomConsole devices
	     */
	    if (CommonMethods.isAtomSyncAvailable(device, tapEnv)) {
		tapEnv.executeCommandUsingSsh(device,
			BroadBandTestConstants.COMMAND_TO_GET_SELF_HEAL_LOGS_FOR_PROCESS_CRASH);
	    } else {
		tapEnv.executeCommandUsingSsh(device,
			BroadBandTestConstants.COMMAND_TO_GET_SELF_HEAL_AGGRESSIVE_LOGS_FOR_PROCESS_CRASH);
	    }
	    /** Executing command to bring interface brlan0 to down */
	    response = tapEnv.executeCommandUsingSsh(device, BroadBandCommandConstants.CMD_BRLAN0_DOWN);
	    tapEnv.waitTill(BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
	    if (CommonMethods.isNull(response)) {
		LOGGER.info("Command to bring interface brlan0 down executed successfully.");
		interfaceBrlan0Status = BootTimeUtils.verifyInterfaceBrlan0UpStatus(device, tapEnv);
		status = CommonMethods.isNotNull(interfaceBrlan0Status) && interfaceBrlan0Status
			.equalsIgnoreCase(BroadBandConnectedClientTestConstants.RADIO_STATUS_DOWN);
	    } else {
		LOGGER.error("Command to bring interface brlan0 down executed successfully.");
	    }
	    if (status) {
		LOGGER.info("STEP 4: ACTUAL : Interface brlan0 is brought to down status.");
	    } else {
		LOGGER.error("STEP 4: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

	    stepNum = "S5";
	    errorMessage = "Interface brlan0 is not coming up through selfheal.";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 5: DESCRIPTION : Verify interface brlan0 interface comes up through selfheal.");
	    LOGGER.info(
		    "STEP 5: ACTION : Check brlan0 status at every 30 seconds of interval till 15 min or till interface is up.");
	    LOGGER.info("STEP 5: EXPECTED : Interface brlan0 should be up through selfheal within 15 min.");
	    LOGGER.info("**********************************************************************************");
	    do {
		interfaceBrlan0Status = BootTimeUtils.verifyInterfaceBrlan0UpStatus(device, tapEnv);
		status = CommonMethods.isNotNull(interfaceBrlan0Status) && interfaceBrlan0Status
			.equalsIgnoreCase(BroadBandConnectedClientTestConstants.RADIO_STATUS_UP);
		if (status) {
		    LOGGER.info("Interface brlan0 came up through selfheal.");
		    break;
		}
		LOGGER.info("INTERFACE brlan0 IS NOT UP YET. WAITING FOR ANOTHER ONE MINUTE.");
		count++;
		tapEnv.waitTill(BroadBandTestConstants.ONE_MINUTE_IN_MILLIS);
	    } while (count <= Integer.parseInt(BroadBandTestConstants.STRING_VALUE_20));
	    if (status) {
		LOGGER.info("STEP 5: ACTUAL : Interface brlan0 came up through selfheal.");
	    } else {
		LOGGER.error("STEP 5: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

	    stepNum = "S6";
	    errorMessage = "Unable to verify required log in /rdklogs/logs/SelfHealAggressive.txt.0 log file.";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 6: DESCRIPTION : Verify required logs in SelfHealAggressive.txt.0 file.");
	    LOGGER.info(
		    "STEP 6: ACTION : Check /rdklogs/logs/SelfHealAggressive.txt.0 file for log <Either brlan0 or l2sd0.100 is not completely up, setting event to recreate vlan and brlan0 interface>.");
	    LOGGER.info(
		    "STEP 6: EXPECTED : Required log mentioned above should be present in /rdklogs/logs/SelfHealAggressive.txt.0 log file.");
	    LOGGER.info("**********************************************************************************");
	    response = tapEnv.executeCommandUsingSsh(device, BroadBandTestConstants.COMMAND_TO_GET_PROCESS_CRASH_LOGS);
	    status = CommonMethods.isNotNull(response)
		    && (CommonMethods.patternMatcher(response, BroadBandTestConstants.INTERFACE_BRLAN_0_SELFHEAL_LOG_01)
			    || CommonMethods.patternMatcher(response,
				    BroadBandTestConstants.INTERFACE_BRLAN_0_SELFHEAL_LOG_02)
			    || CommonMethods.patternMatcher(response,
				    BroadBandTestConstants.INTERFACE_BRLAN_0_SELFHEAL_LOG_03));
	    if (status) {
		LOGGER.info(
			"STEP 6: ACTUAL : Required log verified succesfully in /rdklogs/logs/SelfHealAggressive.txt.0 log file.");
	    } else {
		LOGGER.error("STEP 6: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    stepNum = "S7";
	    errorMessage = "Interface brlan0 is not assigned with valid DHCPv4 address.";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 7: DESCRIPTION : Verify whether brlan0 is assigned properly with valid DHCPv4 address.");
	    LOGGER.info(
		    "STEP 7: ACTION : SSH the device and Execute the following command:<ifconfig brlan0> and verify Ipv4 address.");
	    LOGGER.info("STEP 7: EXPECTED : Interface brlan0 should be assigned with valid DHCPv4 address.");
	    LOGGER.info("**********************************************************************************");
	    status = BootTimeUtils.verifyIpAddressOfInterfaceBrlan0AfterSelfHeal(device,
		    BroadBandTestConstants.String_CONSTANT_IPV4, tapEnv);
	    if (status) {
		LOGGER.info("STEP 7: ACTUAL : Interface brlan0 is assigned with valid DHCPv4 address.");
	    } else {
		LOGGER.error("STEP 7: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    stepNum = "S8";
	    errorMessage = "Interface brlan0 is not assigned with valid DHCPv6 address.";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 8: DESCRIPTION : Verify whether brlan0 is assigned properly with valid DHCPv6 address.");
	    LOGGER.info(
		    "STEP 8: ACTION : SSH the device and Execute the following command:<ifconfig brlan0> and verify Ipv6 address.");
	    LOGGER.info("STEP 8: EXPECTED : Interface brlan0 should be assigned with valid DHCPv6 address.");
	    LOGGER.info("**********************************************************************************");
	    status = BootTimeUtils.verifyIpAddressOfInterfaceBrlan0AfterSelfHeal(device,
		    BroadBandTestConstants.String_CONSTANT_IPV6, tapEnv);
	    if (status) {
		LOGGER.info("STEP 8: ACTUAL : Interface brlan0 is assigned with valid DHCPv6 address.");
	    } else {
		LOGGER.error("STEP 8: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	} catch (Exception exception) {
	    LOGGER.error("Exception occured.");
	    errorMessage = errorMessage + exception.getMessage();
	    LOGGER.error(errorMessage);
	    CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNum, status, errorMessage,
		    false);
	} finally {
	    // Removing sample.txt file
	    tapEnv.executeCommandUsingSsh(device, BroadBandTestConstants.REMOVE_SAMPLE_TEXT_FILE);
	}
	LOGGER.info("ENDING TEST CASE: TC-RDKB-REBOOT-5003");
    }
}
