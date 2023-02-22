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
package com.automatics.rdkb.tests.syndicatepartner;

import java.util.ArrayList;
import java.util.List;

import org.codehaus.jettison.json.JSONObject;
import org.testng.annotations.Test;

import com.automatics.annotations.TestDetails;
import com.automatics.constants.DataProviderConstants;
import com.automatics.device.Dut;
import com.automatics.enums.ExecutionStatus;
import com.automatics.rdkb.BroadBandTestGroup;
import com.automatics.rdkb.constants.BroadBandCommandConstants;
import com.automatics.rdkb.constants.BroadBandTelemetryConstants;
import com.automatics.rdkb.constants.BroadBandTelemetryConstants.TelemetryData;
import com.automatics.rdkb.constants.BroadBandTestConstants;
import com.automatics.rdkb.constants.BroadBandWebPaConstants;
import com.automatics.rdkb.utils.BroadbandPropertyFileHandler;
import com.automatics.rdkb.utils.CommonUtils;
import com.automatics.rdkb.utils.DeviceModeHandler;
import com.automatics.rdkb.utils.telemetry.BroadBandTelemetryUtils;
import com.automatics.rdkb.utils.webpa.BroadBandWebPaUtils;
import com.automatics.rdkb.utils.wifi.BroadBandWiFiUtils;
import com.automatics.rdkb.webui.BroadBandWebUiBaseTest;
import com.automatics.tap.AutomaticsTapApi;
import com.automatics.utils.CommonMethods;

/**
 * Test class for verifying the exclusive features of syndicate partners
 * 
 * @author Vignesh
 *
 */

public class BroadBandSyndicatePartnerTest extends BroadBandWebUiBaseTest {
	

	/** Variable to store no. of telemetry parameters */
	private static final int MINIMUM_NUMBER_OF_TELEMETRY_PAYLOAD_PARAMETERS = 5;

	/**
	 * Test to Verify Partner ID and erouter ips in Telemetry Log
	 * <ol>
	 * <li>Verify whether DCM_LOG_SERVER_URLis present in /etc/dcm.properties
	 * and it points to endpoint.</li>
	 * <li>Reboot the device</li>
	 * <li>Wait till the device is up and Validate Process</li>
	 * <li>Verify whether all must have fields are populated in searchResults
	 * sent to telemetry server from dcmscript.log</li>
	 * <li>Verify whether device populates \"erouterIpv4\" details as part of
	 * Data sent to Telemetry Server.</li>
	 * <li>Verify whether device populates \"erouterIpv6\" details as part of
	 * Data sent to Telemetry Server.</li>
	 * <li>Verify whether device populates \"PartnerId\" details as part of
	 * Data sent to Telemetry Server</li>
	 * </ol>
	 * 
	 * @param Dut
	 * @author Kiruthiga S
	 * @refactor Rakesh C N
	 */
	@Test(enabled = true, dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, groups = BroadBandTestGroup.SYSTEM)
	@TestDetails(testUID = "TC-RDKB-SYNDICATION-1002")
	public void testToVerifyErouterIPsAndPartnerIP(Dut settop) {

		// Variable Declaration begins
		String testCaseId = "TC-RDKB-SYNDICATION-102";
		String stepNum = "S1";
		String errorMessage = null;
		boolean status = false;
		String response = null;
		// Variable Declation Ends

		LOGGER.info("#######################################################################################");
		LOGGER.info("STARTING TEST CASE: TC-RDKB-SYNDICATION-1002");
		LOGGER.info("TEST DESCRIPTION: Test to Verify Partner ID and erouter ips in Telemetry Log ");

		LOGGER.info("TEST STEPS : ");
		LOGGER.info("1. Verify whether DCM_LOG_SERVER_URLis present in /etc/dcm.properties and it points to DCM endpoint.");
		LOGGER.info("2. Reboot the device");
		LOGGER.info("3. Wait till the device is up and Validate Process");
		LOGGER.info("4. Verify whether all must have fields are populated in searchResults sent to telemetry server from dcmscript.log");
		LOGGER.info("5. Verify whether device populates \"erouterIpv4\" details as part of  Data sent to Telemetry Server.");
		LOGGER.info("6. Verify whether device populates \"erouterIpv6\" details as part of  Data sent to Telemetry Server.");
		LOGGER.info("7. Verify whether device populates \"PartnerId\" details as part of  Data sent to Telemetry Server");

		LOGGER.info("#######################################################################################");

		try {
			stepNum = "S1";
			errorMessage = "Failed to verify the DCM Log Server Url in /etc/dcm.properties";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 1: DESCRIPTION : Verify whether DCM_LOG_SERVER_URLis present in /etc/dcm.properties and it points to DCM endpoint.");
			LOGGER.info("STEP 1: ACTION : Execute COMMANDS : rm -rf /nvram/.DCMSettings.conf  /tmp/DCMSettings.conf /nvram/dcm.properties");
			LOGGER.info("STEP 1: EXPECTED : DCM endpoint (DCM_LOG_SERVER_URL) should present in /etc/dcm.properties and it endpoint with HTTPS");
			LOGGER.info("**********************************************************************************");
			tapEnv.executeCommandUsingSsh(settop,
					BroadBandCommandConstants.CMD_TO_FORCE_REMOVE_DCM_SETTINGS);
			status = BroadBandTelemetryUtils.verifyLogServerUrl(settop, tapEnv,
					BroadBandTelemetryConstants.DCM_PROPERTIES_FILE_ETC_FOLDER,
					AutomaticsTapApi.getSTBPropsValue(BroadBandTelemetryConstants.PROP_KEY_DEFAULT_XCONF_LOGUPLOAD_URL));
			LOGGER.info("Status of verification of DCM Log Server Url in /etc/dcm.properties: "
					+ status);
			if (status) {
				LOGGER.info("STEP 1: ACTUAL : DCM endpoint is present in /etc/dcm.properties");
			} else {
				LOGGER.error("STEP 1: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(settop, testCaseId, stepNum, status,
					errorMessage, true);

			stepNum = "S2";
			errorMessage = "Failed to initiate device reboot";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 2: DESCRIPTION : Reboot the device");
			LOGGER.info("STEP 2: ACTION : Execute Reboot command");
			LOGGER.info("STEP 2: EXPECTED : Device should reboot automatically");
			LOGGER.info("**********************************************************************************");
			status = BroadBandWiFiUtils
					.setWebPaParams(
							settop,
							BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_CONTROL_DEVICE_REBOOT,
							BroadBandTestConstants.DEVICE,
							BroadBandTestConstants.CONSTANT_0);
			if (status) {
				LOGGER.info("STEP 2: ACTUAL : Device Rebooted succesfully");
			} else {
				LOGGER.error("STEP 2: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(settop, testCaseId, stepNum, status,
					errorMessage, true);

			stepNum = "S3";
			errorMessage = "Failed to access device after WebPa reboot";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 3: DESCRIPTION : Wait till the device is up and Validate Process");
			LOGGER.info("STEP 3: ACTION : Device should be reachable ");
			LOGGER.info("STEP 3: EXPECTED : Device should be accessible after reboot");
			LOGGER.info("**********************************************************************************");
//			if (CommonMethods.isSTBRebooted(tapEnv, settop, BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS,
//			BroadBandTestConstants.CONSTANT_6)) {  // temporarily conmmented
		errorMessage = "Device did not come up after webpa reboot";
		status = CommonMethods.waitForEstbIpAcquisition(tapEnv, settop);
//	}
			if (status) {
				LOGGER.info("STEP 3: ACTUAL : Device is accessible after reboot");
			} else {
				LOGGER.error("STEP 3: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(settop, testCaseId, stepNum, status,
					errorMessage, true);

			stepNum = "S4";
			errorMessage = "Device not populating all mandatory data fields in telemetry Data";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 4: DESCRIPTION : Verify whether all must have fields are populated in searchResults sent to telemetry server from dcmscript.log");
			LOGGER.info("STEP 4: ACTION : Execute cat dcmscript.log and verify all mandatory fields are there in rdklogs");
			LOGGER.info("STEP 4: EXPECTED : Device should populate required telemetry data and upload to telemetry server.");
			LOGGER.info("**********************************************************************************");

			boolean isTelemetry2Enabled = BroadBandWebPaUtils
					.getAndVerifyWebpaValueInPolledDuration(
							settop,
							tapEnv,
							BroadBandWebPaConstants.WEBPA_PARAM_FOR_TELEMETRY_2_0_ENABLE,
							BroadBandTestConstants.TRUE,
							BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS,
							BroadBandTestConstants.TEN_SECOND_IN_MILLIS);
			

			List<String> telemetryJsonPayLoad = new ArrayList<String>();
			if (BroadbandPropertyFileHandler.isSplunkEnabled()
					&& isTelemetry2Enabled) {

				telemetryJsonPayLoad = BroadBandTelemetryUtils
						.getTelemetryDataPayloadGeneratedFromSplunk(settop,
								tapEnv);
			} else {
				telemetryJsonPayLoad = BroadBandTelemetryUtils
						.getTelemetryDataPayloadGeneratedFromDcmScriptLog(
								settop, tapEnv);
			}

			JSONObject telemetryData = BroadBandTelemetryUtils
					.getTelemetryDataPayLoadAsJsonFormat(settop, tapEnv,
							telemetryJsonPayLoad);
			status = telemetryData.length() >= MINIMUM_NUMBER_OF_TELEMETRY_PAYLOAD_PARAMETERS;
			if (status) {
				LOGGER.info("STEP 4: ACTUAL : All Mandatory fields are populated and uploaded to telemetry server");
			} else {
				LOGGER.error("STEP 4: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(settop, testCaseId, stepNum, status,
					errorMessage, true);

			stepNum = "S5";
			errorMessage = "Device populating wrong or no erouterIpv4  details ";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 5: DESCRIPTION : Verify whether device populates erouterIpv4 details as part of  Data sent to Telemetry Server and cross validate with erouter0 interface");
			LOGGER.info("STEP 5: ACTION : Execute cat dcmscript.log , ifconfig erouter0 and verify the erouterIpv4 in rdklogs with erouter0 interface");
			LOGGER.info("STEP 5: EXPECTED : Device should populates erouterIpv4 details as part of  Data sent to Telemetry Server");
			LOGGER.info("**********************************************************************************");
			String actualErouterIpv4Address = telemetryData
					.getString(BroadBandTelemetryConstants.JSON_MARKER_IPV4_ADDRESS);
			LOGGER.info(actualErouterIpv4Address);
			String ifConfigErouterResponse = tapEnv.executeCommandUsingSsh(
					settop, BroadBandTestConstants.IFCONFIG_EROUTER);
			String expectedErouterIpv4Address = CommonMethods.patternFinder(
					ifConfigErouterResponse,
					BroadBandTestConstants.INET_V4_ADDRESS_PATTERN);
			status = CommonMethods.isNotNull(expectedErouterIpv4Address)
					&& expectedErouterIpv4Address
							.equalsIgnoreCase(actualErouterIpv4Address);
			if (status) {
				LOGGER.info("STEP 5: ACTUAL : erouterIpv4 is populated in the telemetry logs");
			} else {
				LOGGER.error("STEP 5: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(settop, testCaseId, stepNum, status,
					errorMessage, true);

			stepNum = "S6";
			errorMessage = "Device populating wrong or no erouterIpv6  details ";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 6: DESCRIPTION : Verify whether device populates erouterIpv6 details as part of  Data sent to Telemetry Server and cross validate with erouter0 interface");
			LOGGER.info("STEP 6: ACTION : Execute cat dcmscript.log , ifconfig erouter0 and verify the erouterIpv6 in rdklogs with erouter0 interface");
			LOGGER.info("STEP 6: EXPECTED : Device should populates erouterIpv6 details as part of  Data sent to Telemetry Server");
			LOGGER.info("**********************************************************************************");
			String actualErouterIpv6Address = telemetryData
					.getString(BroadBandTelemetryConstants.JSON_MARKER_IPV6_ADDRESS);

			if (!DeviceModeHandler.isRPIDevice(settop)) {
				LOGGER.info(actualErouterIpv6Address);
				String expectedErouterIpv6Address = CommonMethods.patternFinder(ifConfigErouterResponse,
						BroadBandTestConstants.INET_V6_ADDRESS_PATTERN);
				status = CommonMethods.isNotNull(expectedErouterIpv6Address)
						&& expectedErouterIpv6Address.equalsIgnoreCase(actualErouterIpv6Address);
				if (status) {
					LOGGER.info("STEP 6: ACTUAL :  erouterIpv6 is populated in the telemetry logs");
				} else {
					LOGGER.error("STEP 6: ACTUAL : " + errorMessage);
				}
				LOGGER.info("**********************************************************************************");
				tapEnv.updateExecutionStatus(settop, testCaseId, stepNum, status, errorMessage, true);
			} else {
				LOGGER.info("IPv6 is not enabled for RPI device : Skipping teststep ...");
				tapEnv.updateExecutionForAllStatus(settop, testCaseId, stepNum, ExecutionStatus.NOT_APPLICABLE,
						errorMessage, false);
			}

			stepNum = "S7";
			errorMessage = "Device populating wrong or no PartnerId details";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 7: DESCRIPTION : Verify whether device populates PartnerId details as part of  Data sent to Telemetry Server and validate with webpa ");
			LOGGER.info("STEP 7: ACTION : Execute cat dcmscript.log , Param : Device.DeviceInfo.X_RDKCENTRAL-COM_Syndication.PartnerId and verify the PartnerId in rdklogs with webpa");
			LOGGER.info("STEP 7: EXPECTED : Device should populates PartnerId details as part of  Data sent to Telemetry Server");
			LOGGER.info("**********************************************************************************");
			status = BroadBandTelemetryUtils.verifyTelemetryPayLoadParamameter(
					settop, tapEnv, telemetryData, TelemetryData.PARTNER_ID);
			if (status) {
				LOGGER.info("STEP 7: ACTUAL :  PartnerId is populated in the telemetry logs");
			} else {
				LOGGER.error("STEP 7: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(settop, testCaseId, stepNum, status,
					errorMessage, true);

		} catch (Exception e) {
			errorMessage = errorMessage + e.getMessage();
			LOGGER.error(errorMessage);
			CommonUtils.updateTestStatusDuringException(tapEnv, settop,
					testCaseId, stepNum, status, errorMessage, false);
		}
		LOGGER.info("ENDING TEST CASE: TC-RDKB-SYNDICATION-1002");
	}

}
